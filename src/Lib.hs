{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Control.Concurrent.MVar
import Control.Concurrent
import Network
import System.IO
import System.Exit
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.PubKey.RSA as RSA
import qualified Data.ByteString as BS
import Control.Eff
import Control.Monad
import qualified Data.Map.Lazy as Map
import Data.Bits

import Civskell.Tech.Network
import Civskell.Tech.Encrypt

import Civskell.Data.Types
import Civskell.Data.Player
import Civskell.Data.World
import Civskell.Data.Logging

import qualified Civskell.Packet.Clientbound as Client
import qualified Civskell.Packet.Serverbound as Server

-- Send new connections to connLoop, and listen on 25565
-- Note for testing: Use virtualbox port forwarding to test locally
startListening :: IO ()
startListening = do
  wor <- newMVar initWorld
  _ <- forkIO (connLoop wor =<< listenOn (PortNumber 25565))
  terminal

terminal :: IO ()
terminal = do
  l <- getLine
  if l == "quit" then exitSuccess else terminal

connLoop :: MVar WorldData -> Socket -> IO ()
connLoop wor sock = do
  -- accept is what waits for a new connection
  (handle, cliHost, cliPort) <- accept sock
  -- Log that we got a connection
  putStrLn $ "Got Client connection: " ++ show cliHost ++ ":" ++ show cliPort
  -- Don't line buffer the network
  hSetBuffering handle NoBuffering
  -- We don't need the thread id
  _ <- forkIO (runM $ runLogger $ runNetworking Nothing Nothing handle $ runWorld wor $ connHandler)
  -- Wait for another connection
  connLoop wor sock

connHandler :: (HasLogging r,HasIO r,HasNetworking r,HasWorld r) => Eff r ()
connHandler = do
  -- Get a packet from the client (should be a handshake)
  mHandshake <- getPacket Handshaking
  maybe (logg "WTF no packet? REEEEEEEEEEEEE") handleHandshake mHandshake

handleHandshake :: (HasLogging r,HasIO r,HasNetworking r,HasWorld r) => Server.Packet -> Eff r ()
-- Normal handshake recieved
handleHandshake (Server.Handshake protocol addr port newstate) = do
  -- Log to console that we have a handshaking connection
  logg $ "Handshake: "
  logg $ "  Protocol: " ++ show protocol
  logg $ "  Server Address: " ++ addr ++ ":" ++ show port
  logg $ "  Next State: " ++ (case newstate of {1 -> "Status"; 2 -> "Login"; _ -> "Invalid"})
  case newstate of
    -- 1 for status
    1 -> statusMode
    -- 2 for login
    2 -> initiateLogin
    -- otherwise invalid TODO: This is invalid because it might have asked for status and fucked up, but this is only valid if it asks for login
    _ -> sendPacket (Client.Disconnect (jsonyText "Invalid Continuation State"))
-- Non-handshake (this is an error)
handleHandshake pkt = do
  -- Log to console that we have a bad packet
  logLevel ErrorLog $ "Unexpected non-handshake packet with Id: " ++ show (packetId pkt)
  -- Tell client to fuck off because they gave us a bad packet
  sendPacket (Client.Disconnect (jsonyText "Bad Packet D:"))

checkVTandSS :: RSA.PrivateKey -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
checkVTandSS priv vtFromClient ssFromClient actualVT = do
  -- Try to decrypt their vt response
  case RSA.decrypt Nothing priv vtFromClient of
    -- if it fails, print the error
    Left e -> Left $ "Failed to parse Verify Token: " ++ show e
    Right vtHopefully -> do
      -- if it succeeds, check that it is actually the original vt
      if vtHopefully /= actualVT
        -- if it isn't, throw a fit and disconnect the client
        then Left "Invalid Verify Token"
        -- If the verify token was ok, then decrypt the ss
        else case RSA.decrypt Nothing priv ssFromClient of
          -- If it fails, print the error
          Left e -> Left $ "Failed to parse Shared Secret: " ++ show e
          Right ss -> Right ss

encryptionPhase :: (HasIO r,HasLogging r,HasNetworking r,HasWorld r) => String -> Eff r ()
encryptionPhase nameFromLoginStart = do
  -- Get a new keypair to use with this client during the key exchange
  (pub,priv) <- getAKeypair
  -- Verify Token is fixed because why not
  let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
  -- sId is blank because (((history)))
  let sId = ""
  -- encode our public key into ASN.1's serialized format
  let encPubKey = encodePubKey pub
  -- Send an encryption request to the client
  sendPacket (Client.EncryptionRequest sId encPubKey vt)
  -- Wait for them to send an Encryption Response
  getPacket LoggingIn >>= \case
    Just (Server.EncryptionResponse ssFromClient vtFromClient) -> do
      case checkVTandSS priv vtFromClient ssFromClient vt of
        Left s -> sendPacket (Client.Disconnect (jsonyText s)) >> logLevel ErrorLog s
        -- Hand off to authentication, also upgrade to using encryption
        Right ss -> do
          -- make the serverId hash for auth
          let loginHash = genLoginHash sId ss encPubKey
          setupEncryption (makeEncrypter ss, ss, ss)
          authPhase nameFromLoginStart loginHash
          return ()
    Just _ -> logg "Expected EncryptionResponse, got something else"
    Nothing -> logg "unable to parse encryptionresponse packet"

authPhase :: (HasIO r,HasLogging r,HasNetworking r,HasWorld r) => String -> String -> Eff r ()
authPhase name hash = do
  -- do the Auth stuff with Mojang
  authGetReq name hash >>= \case
    -- If the auth is borked, its probably Mojangs fault tbh
    Nothing -> do
      logLevel ErrorLog "Parse error on auth"
      -- Disclaim guilt
      sendPacket (Client.Disconnect $ jsonyText "Auth failed (not Lazersmoke's fault, probably!)")
      return ()
    Just (uuid,nameFromAuth) -> do
      -- Tell the client to compress starting now. Agressive for testing
      -- EDIT: setting this to 3 gave us a bad frame exception :S
      -- TODO: merge this packet into `setCompression`
      sendPacket (Client.SetCompression 16)
      setCompression $ Just 16
      -- Send a login success. We are now in play mode
      sendPacket (Client.LoginSuccess uuid nameFromAuth)
      logg "Yay it all worked! (maybe); starting play cycle"
      startPlaying

-- Start Play Mode, where we have just sent "Login Success"
startPlaying :: (HasWorld r, HasLogging r, HasNetworking r) => Eff r ()
startPlaying = initPlayer $ do
  -- EID, Gamemode (Enum), Dimension (Enum), Difficulty (Enum), Max Players (deprecated), Level Type, reduce debug info?
  sendPacket (Client.JoinGame 0 Survival 0 Peaceful 100 "default" False)
  -- Tell them we aren't vanilla so no one gets mad at mojang for our fuck ups
  sendPacket (Client.PluginMessage "MC|Brand" (serialize "civskell"))
  -- Difficulty to peacful
  sendPacket (Client.ServerDifficulty Peaceful)
  -- Home spawn position
  sendPacket (Client.SpawnPosition (BlockCoord (0,64,0)))
  -- Player Abilities
  sendPacket (Client.PlayerAbilities 0x00 0.0 1.0)
  -- Send initial world
  let exampleColumn = replicate 8 exampleChunk ++ replicate 8 emptyChunk
  forM_ [0..48] $ \x -> setColumn exampleColumn ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
  -- 0 is player inventory
  flushInbox
  do
    sendPacket (Client.WindowItems 0 (replicate 45 EmptySlot))
    setInventorySlot 4 (Slot 1 5 0 Nothing)
    packetLoop

sendChunk :: (HasWorld r, HasNetworking r, HasLogging r) => ChunkCoord -> Eff r ()
sendChunk cc@(ChunkCoord (x,y,z)) = do
  c <- getChunk cc
  sendPacket (Client.ChunkData (fromIntegral x,fromIntegral z) False (bit y) [c] Nothing [])

sendCol :: (HasWorld r, HasNetworking r, HasLogging r) => (Int,Int) -> Maybe BS.ByteString -> Eff r ()
sendCol (cx,cz) mbio = do
  cs <- forM [0..15] $ \cy -> getChunk (ChunkCoord (cx,cy,cz))
  sendPacket (Client.ChunkData (fromIntegral cx,fromIntegral cz) True (bitMask cs) (filter (not . isAirChunk) cs) mbio [])
  where
    -- [Bool] -> VarInt basically
    bitMask as = foldl (\i b -> fromBool b .|. shiftL i 1) 0 (map isAirChunk as)
    fromBool True = 1
    fromBool False = 0

isAirChunk :: ChunkSection -> Bool
isAirChunk (ChunkSection m) = Map.null m

emptyChunk :: ChunkSection
emptyChunk = ChunkSection Map.empty

exampleChunk :: ChunkSection
exampleChunk = ChunkSection $ Map.fromList [(BlockCoord (x,y,z),stone) | x <- [0..15], y <- [0..15], z <- [0..15]]
  where
    stone = BlockState 1 0

packetLoop :: (HasPlayer r, HasLogging r, HasNetworking r, HasWorld r) => Eff r ()
packetLoop = do
  mPkt <- getPacket Playing
  flushInbox
  maybe (logLevel ErrorLog "Failed to parse incoming packet") gotPacket mPkt
  packetLoop

gotPacket :: (HasPlayer r, HasLogging r, HasWorld r) => Server.Packet -> Eff r ()
gotPacket (Server.PluginMessage "MC|Brand" cliBrand) = do
  --logg $ "Client brand is: " ++ show (BS.tail cliBrand)
  setBrand $ show (BS.tail cliBrand)
gotPacket (Server.ClientSettings loc viewDist chatMode chatColors skin hand) = do
  logg $ "Client Settings:"
  logg $ "  Locale: " ++ loc
  logg $ "  View Distance: " ++ show viewDist
  logg $ "  Chat Mode: " ++ show chatMode
  logg $ "  Chat colors enabled: " ++ show chatColors
  logg $ "  Skin bitmask: " ++ show skin
  logg $ "  Main Hand: " ++ show hand
  -- 0x00 means all absolute (It's a relativity flag bitfield)
  pendTeleport (1.0,130.0,1.0) (0.0,0.0) 0x00
  --logg $ "Teleport with id " ++ show tid ++ " pending"
gotPacket (Server.TPConfirm tid) = do
  c <- clearTeleport tid
  if c
    then logg $ "Client confirms teleport with id: " ++ show tid
    else logg $ "Client provided bad teleport id: " ++ show tid
gotPacket (Server.Animation _anim) = return ()
gotPacket (Server.HeldItemChange slotNum) = do
  logg $ "Player is holding slot number " ++ show slotNum
  setHolding slotNum
gotPacket (Server.ClientStatus _status) = return ()
  --logg $ "Client Status is: " ++ (case status of {0 -> "Perform Respawn"; 1 -> "Request Stats"; 2 -> "Open Inventory"})
gotPacket (Server.CreativeInventoryAction slotNum slotDat) = do
  logg $ "Player creatively set slot " ++ show slotNum ++ " to {" ++ show slotDat ++ "}"
  -- TODO: This will echo back a SetSlot packet, add another way to access the effect inventory
  --setInventorySlot (clientToCivskellSlot slotNum) slotDat
gotPacket (Server.KeepAlive kid) = do
  c <- clearKeepAlive kid
  if c
    then logg $ "Player sent keep alive pong with id: " ++ show kid
    else logg $ "Player sent bad keep alive id: " ++ show kid
gotPacket (Server.PlayerPosition (x,y,z) grounded) = do
  logg "Sending keep alive"
  -- Counting is an effect
  pendKeepAlive
  setPlayerPos (x,y,z)
  logLevel VerboseLog $ "Player is at " ++ show (x,y,z)
  logLevel VerboseLog $ if grounded then "Player is on the ground" else "Player is not on the ground"
gotPacket (Server.PlayerPositionAndLook (x,y,z) (yaw,pitch) grounded) = do
  setPlayerPos (x,y,z)
  setPlayerViewAngle (yaw,pitch)
  logLevel VerboseLog $ "Player is at " ++ show (x,y,z)
  logLevel VerboseLog $ "Player is looking to " ++ show (yaw,pitch)
  logLevel VerboseLog $ if grounded then "Player is on the ground" else "Player is not on the ground"
gotPacket (Server.PlayerLook (y,p) grounded) = do
  setPlayerViewAngle (y,p)
  logLevel VerboseLog $ "Player is looking to " ++ show (y,p)
  logLevel VerboseLog $ if grounded then "Player is on the ground" else "Player is not on the ground"
gotPacket (Server.Player grounded) = do
  logLevel VerboseLog $ if grounded then "Player is on the ground" else "Player is not on the ground"
gotPacket (Server.CloseWindow wid) = do
  logg $ "Player is closing a window with id: " ++ show wid
gotPacket (Server.ChatMessage msg) = case msg of
  "/gamemode 1" -> setGamemode Creative
  "/gamemode 0" -> setGamemode Survival
  _ -> logg $ "Player said: " ++ msg
gotPacket p@(Server.PlayerDigging action) = case action of
  StartDig block _side -> do
    logg $ "Player started digging block: " ++ show block
    -- Instant Dig
    removeBlock block
  SwapHands -> do
    logg $ "Player is swapping their hands"
    heldSlot <- getHolding
    heldItem <- getInventorySlot heldSlot
    offItem <- getInventorySlot 45
    setInventorySlot heldSlot offItem
    setInventorySlot 45 heldItem
  _ -> logLevel ErrorLog $ "Unhandled Player Dig Action: " ++ show p
gotPacket (Server.PlayerBlockPlacement block side hand _cursorCoord) = do
  logg $ "Player is placing a block"
  heldSlot <- if hand == MainHand then getHolding else pure 45
  heldItem <- getInventorySlot heldSlot
  logg $ "Slot " ++ show heldSlot ++ " is " ++ show heldItem
  case heldItem of
    EmptySlot -> logg "Player is tring to place air"
    Slot bid count dmg nbt -> do
      -- Remove item from inventory
      let newSlot = if count == 1 then EmptySlot else (Slot bid (count - 1) dmg nbt)
      setInventorySlot heldSlot newSlot
      let blockDmg = 0
      -- Updates client as well
      setBlock (BlockState bid blockDmg) (blockOnSide block side)
  -- WId SlotNum "Button" TransactionId InventoryMode ItemInSlot
gotPacket (Server.ClickWindow wid slotNum _transId mode _slot) = if wid /= 0 then logLevel ErrorLog "Non-player inventories not supported" else case mode of
  -- TODO: optimize and combine cases; right-click usually just means to move one item, but we consider it an entirely separate case here
  NormalClick rClick -> do
    actualSlot <- getInventorySlot (clientToCivskellSlot slotNum)
    currHeld <- getInventorySlot (-1)
    case actualSlot of
      EmptySlot -> case currHeld of
        -- Both empty -> No-op
        EmptySlot -> pure ()
        -- Putting something in an empty slot
        Slot currbid currcount currdmg currnbt -> if rClick 
          then do
            let newHeld = if currcount == 1 then EmptySlot else Slot currbid (currcount - 1) currdmg currnbt
            setInventorySlot (-1) newHeld
            let placed = Slot currbid 1 currdmg currnbt
            setInventorySlot (clientToCivskellSlot slotNum) placed
          else do
            setInventorySlot (-1) EmptySlot
            setInventorySlot (clientToCivskellSlot slotNum) currHeld
      Slot actbid actcount actdmg actnbt -> case currHeld of
        -- Picking something up into an empty hand
        EmptySlot -> if rClick
          then do
            let picked = Slot actbid (actcount - (actcount `div` 2)) actdmg actnbt
            setInventorySlot (-1) picked
            let left = Slot actbid (actcount `div` 2) actdmg actnbt
            setInventorySlot (clientToCivskellSlot slotNum) left
          else do
            setInventorySlot (-1) actualSlot
            setInventorySlot (clientToCivskellSlot slotNum) EmptySlot
        -- Two item stacks interacting
        Slot currbid currcount currdmg currnbt -> if currbid == actbid && currdmg == actdmg && currnbt == actnbt
          -- Like stacks; combine
          then if rClick 
            then if currcount + 1 > 64
              then pure ()
              else do
                setInventorySlot (clientToCivskellSlot slotNum) (Slot actbid (actcount + 1) actdmg actnbt)
                let stillHeld = if currcount == 1 then EmptySlot else Slot currbid (currcount - 1) currdmg currnbt
                setInventorySlot (-1) stillHeld
            else if currcount + actcount > 64 
              then do
                let delta = 64 - actcount
                setInventorySlot (-1) (Slot currbid (currcount - delta) currdmg currnbt)
                setInventorySlot (clientToCivskellSlot slotNum) (Slot actbid 64 actdmg actnbt)
              else do
                setInventorySlot (-1) EmptySlot
                setInventorySlot (clientToCivskellSlot slotNum) (Slot actbid (currcount + actcount) actdmg actnbt)
          -- Unlike stacks; swap
          else do
            setInventorySlot (-1) actualSlot
            setInventorySlot (clientToCivskellSlot slotNum) currHeld
  -- Right click is exactly the same as left
  ShiftClick _rClick -> pure ()
  NumberKey _num -> pure ()
  MiddleClick -> pure ()
  ItemDropOut _isStack -> pure ()
  -- "Painting" mode
  PaintingMode _mode -> logLevel ErrorLog "Painting mode not supported"
  -- Double click
  DoubleClick -> logLevel ErrorLog "Double click not supported"

gotPacket a = logLevel ErrorLog $ "Unsupported packet: " ++ show a

blockOnSide :: BlockCoord -> BlockFace -> BlockCoord
blockOnSide (BlockCoord (x,y,z)) Bottom = BlockCoord (x,y-1,z)
blockOnSide (BlockCoord (x,y,z)) Top = BlockCoord (x,y+1,z)
blockOnSide (BlockCoord (x,y,z)) North = BlockCoord (x,y,z-1)
blockOnSide (BlockCoord (x,y,z)) South = BlockCoord (x,y,z+1)
blockOnSide (BlockCoord (x,y,z)) West = BlockCoord (x-1,y,z)
blockOnSide (BlockCoord (x,y,z)) East = BlockCoord (x+1,y,z)

-- Do the login process
initiateLogin :: (HasIO r,HasLogging r,HasNetworking r,HasWorld r) => Eff r ()
initiateLogin = do
  -- Get a login start packet from the client
  mloginStart <- getPacket LoggingIn
  case mloginStart of
    -- LoginStart packets contain their username as a String
    Just (Server.LoginStart name) -> do
      -- Log that they connected
      logg $ name ++ " is logging in"
      encryptionPhase name
    Just _ -> logg "Unexpected non-login start packet"
    Nothing -> logg "Unable to parse login start packet"

-- Server list refresh ping/status cycle
statusMode :: (HasLogging r,HasNetworking r,HasWorld r) => Eff r ()
statusMode = do
  -- Wait for them to ask our status
  mpkt <- getPacket Status
  case mpkt of
    -- Make sure they are actually asking for our status
    Just Server.StatusRequest -> do
      -- TODO: dynamic response
      playerCount <- length <$> allPlayers
      let resp = "{\"version\":{\"name\":\"Civskell (1.11.2)\",\"protocol\":316},\"players\":{\"max\": 100,\"online\": " ++ show playerCount ++ "},\"description\":{\"text\":\"An Experimental Minecraft server written in Haskell | github.com/Lazersmoke/civskell\"}}"
      -- Send resp to clients
      sendPacket (Client.StatusResponse resp)
      -- Wait for them to start a ping
      mping <- getPacket Status
      case mping of
        -- Make sure they are actually pinging us, then send a pong right away
        Just (Server.StatusPing l) -> sendPacket (Client.StatusPong l)
        Just _ -> logg "Client sent non-ping packet, disconnecting"
        Nothing -> logg "WTF bruh you got this far and fucked up"
    Just _ -> logg "Client gave non status-request packet, disconnecting"
    Nothing -> logg "WTF m8? you tried to ask for our status, then gave us a shitty packet? Who the fuck do you think I am!?!?!?"

-- Simple way to inject a text message into a json chat string
jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"
