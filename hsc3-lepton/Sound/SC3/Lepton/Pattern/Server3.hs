{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (GHC concurrency, DeriveDataTypeable)

Server to manage patterns, take 3.
Using TVar instead of MVar, not using ReaderMonad.

-}
module Sound.SC3.Lepton.Pattern.Server3 where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import Control.Exception (bracket)
import Data.ByteString.Lazy (ByteString)
import Data.Data (Data, Typeable)

import Data.Binary (decode)
import Sound.OpenSoundControl
import Sound.SC3 (notify, n_free)

import qualified Codec.Compression.Zlib as Z
import qualified Data.Foldable as F
import qualified Data.Map as M

import Sound.SC3.Lepton.Pattern

------------------------------------------------------------------------------
-- * Types

data Protocol = Tcp | Udp deriving (Eq,Show,Data,Typeable)

data ConInfo = ConInfo
  { ciHost :: String
  , ciPort :: Int
  , ciProtocol :: Protocol
  } deriving (Eq, Show)

data Connection = UDPCon UDP | TCPCon TCP

instance Transport Connection where
  send (UDPCon c) o = send c o
  send (TCPCon c) o = send c o
  recv (UDPCon c) = recv c
  recv (TCPCon c) = recv c
  close (UDPCon c) = close c
  close (TCPCon c) = close c

data ServerEnv = ServerEnv
  { seThreads :: TVar (M.Map String ThreadInfo)
  , sePort :: Int
  , seLept :: UDP
  , seSC :: ConInfo }

data ThreadInfo = Running ThreadId | Stopped deriving (Eq, Show)

------------------------------------------------------------------------------
-- * Guts

-- XXX: Why not ReaderT IO?
-- I thought wrapping with monad transformer makes the code slow.
-- But haven't benchmarked and compared, yet.... biased, not good.
--
-- XXX: Why using TVar instead of MVar?
-- Thought STM will work better here, but not much difference.

-- | Guts of server message receiving loop.
--
-- When shutting down, lepton side connection would be closed and
-- all threads in server env will be killed.
--
go ::
  Int
  -- ^ Port of pattern server
  -> String
  -- ^ Host of scsynth to connect
  -> Protocol
  -- ^ Protocol of scsynth to connect
  -> Int
  -- ^ Port of scsynth to connect
  -> IO ()
go lport shost sprtc sport = bracket acquire tidyup work where
  acquire = do
    putStrLn $ "Starting server with port " ++ show lport
    tv <- atomically $ newTVar M.empty
    lcon <- udpServer "127.0.0.1" lport
    let scon = ConInfo shost sport sprtc
    putStrLn $
      concat [ "Using scsynth [host:", shost
             , ", port:", show sport, ", protocol:", show sprtc, "]"]
    return $ ServerEnv tv lport lcon scon
  tidyup e = do
    close $ seLept e
    tmap <- atomically $ readTVar $ seThreads e
    F.forM_ tmap $ \tinfo ->
      case tinfo of
        Running tid -> killThread tid
        Stopped     -> return ()
  work = loop

-- | Guts of server loop.
-- Receives OSC message from lepton side connection, sends message to
-- scsynth.
loop :: ServerEnv -> IO ()
loop env = forever $ do
  msg <- recv $ seLept env
  handleMessage env msg

-- | Handle bundled and non-bundles OSC message.
handleMessage :: ServerEnv -> OSC -> IO ()
handleMessage env msg = case msg of
  Bundle time msgs -> mapM_ (sendMessage env (Just time)) msgs
  _                -> sendMessage env Nothing msg

-- | Pattern match OSC message and send to scsynth server.
sendMessage
  :: ServerEnv
  -- ^ Environment for server
  -> Maybe Time
  -- ^ Offset time of OSC message
  -> OSC
  -- ^ OSC message body
  -> IO ()
sendMessage env time msg = case msg of
  Bundle _ msgs                          -> mapM_ (sendMessage env time) msgs
  Message "/l_new" [String name, Blob b] -> runLNew env time name b
  Message "/l_free" [String name]        -> runLFree env time name
  Message "/l_freeAll" []                -> runLFreeAll env time
  Message "/l_dump" []                   -> runLDump env
  _                                      -> putStrLn $ "Unknown: " ++ show msg

-- | Run new pattern.
runLNew
  :: ServerEnv
  -- ^ Environment for server
  -> Maybe Time
  -- ^ Offset time of OSC message
  -> String
  -- ^ Name of thread
  -> ByteString
  -- ^ Serialized pattern
  -> IO ()
runLNew env time name blob = do
  --
  -- XXX: No gurantee for thread Map to contain un-managed threads.
  --
  -- Since forkIO is invoked between atomic STM actions,
  -- unless using unsafePerformIO, forkIO need to be called outside of
  -- STM action.
  --
  tmap <- atomically $ readTVar $ seThreads env
  let !maybeKill = case M.lookup name tmap of
        Just (Running tid) -> killThread tid
        _                  -> return ()
  tid <- forkIO $ do
    maybePause time
    maybeKill
    mkThread env time name blob
  atomically $ writeTVar (seThreads env) $ M.insert name (Running tid) tmap

-- | Free specified thread.
runLFree
  :: ServerEnv
  -- ^ Environment for server
  -> Maybe Time
  -- ^ Offset time for sending message
  -> String
  -- ^ Thread name to kill
  -> IO ()
runLFree env time name = do
  tmap <- atomically $ readTVar $ seThreads env
  case M.lookup name tmap of
    Just (Running tid) -> do
      maybePause time
      killThread tid
      atomically $
        writeTVar (seThreads env) $ M.delete name tmap
    _                  -> putStrLn $ "Thread " ++ name ++ " does not exist"

-- | Free all thread in server environment.
runLFreeAll
  :: ServerEnv
  -- ^ Server environment
  -> Maybe Time
  -- ^ Offset time for sending message
  -> IO ()
runLFreeAll env time = do
  let tv = seThreads env
  tmap <- atomically $ readTVar tv
  maybePause time
  F.forM_ tmap $ \tinfo -> case tinfo of
    Running tid -> do
      killThread tid
      putStrLn $ unwords ["Thread:", show tid, "killed"]
    _           -> return ()
  atomically $ writeTVar tv $ M.empty

-- | Dump info of given server environment.
runLDump :: ServerEnv -> IO ()
runLDump env = do
  tmap <- atomically $ readTVar $ seThreads env
  putStrLn "========== Threads ==========="
  F.forM_ (M.toList tmap) $ \(name,ti) ->
    putStrLn $ name ++ ": " ++ show ti

-- | Play given patttern with new thread.
-- When the given pattern is finite, forked thread will update thread
-- status of itself in server env when finished playing the pattern.
mkThread
  :: ServerEnv
  -- ^ Environment for server
  -> Maybe Time
  -- ^ Offset time
  -> String
  -- ^ Name for new thread
  -> ByteString
  -- ^ Serialized pattern
  -> IO ()
mkThread env time name blob = case decodePattern blob of
  Left err   -> print err
  Right pat' -> do
    withTransport (fromConInfo (seSC env)) $ \fd ->
      let acquire = do
            send fd (notify True)
            tid <- newNid
            return (fd, tid)
          tidyup (fd',tid) = do
            send fd' $ bundle immediately [notify False, n_free [tid]]
            close fd'
            --
            -- XXX:
            -- When below cleanup were done, creating new thread with same name
            -- will make unmanaged thread in ServerEnv. Commented out for
            -- supporting creation of thread with same name.
            --
            -- atomically $ do
            --   let tv = seThreads env
            --   tmap <- readTVar tv
            --   writeTVar tv (M.delete name tmap)
            --
          work (fd',tid) = do
            time' <- maybe (UTCr `fmap` utcr) return time
            runMsgFrom time' pat' tid fd'
            atomically $ do
              let tv = seThreads env
              tmap <- readTVar tv
              writeTVar tv $ M.adjust (const Stopped) name tmap
      in  bracket acquire tidyup work

-- | Open new connection from host, port, and protocol information.
fromConInfo :: ConInfo -> IO Connection
fromConInfo (ConInfo h p ptc) = case ptc of
  Udp -> UDPCon `fmap` openUDP h p
  Tcp -> TCPCon `fmap` openTCP h p

-- | Pause until given time when Just time was given.
-- Will not pause when Nothng was given, nor given time was past.
maybePause :: Maybe Time -> IO ()
maybePause = F.mapM_ $ \time -> do
  dt <- (as_utcr time -) `fmap` utcr
  let (q,_) = properFraction (dt * 1e6)
  when (dt > 0) $ threadDelay q

-- | Decode compressed pattern bytestring.
decodePattern :: ByteString -> Either String (L () (ToOSC Double))
decodePattern = t2l . decode . Z.decompress
