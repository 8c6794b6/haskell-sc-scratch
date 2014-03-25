{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (GHC concurrency)

Server to manage patterns.

Writing server with using hosc package. Communication between client would
be done with sending and receiving OSC command, instead of raw ByteString.

-}
module Sound.SC3.Lepton.Pattern.Server
  ( -- * Types
    ConInfo(..)
  , Con(..)
  , LeptSeq(..)
  , Protocol(..)
  , ServerEnv(..)
  , ServerLoop(..)
  , Thread(..)
  , ThreadState(..)
    -- * Server actions
  , shutdownServer
  , defaultLeptSeq
  , mkInitEnv
  , runServer
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Reader
import Data.ByteString.Char8 (unpack)
import Data.Data
import Data.Ord (comparing)

import Data.Binary (decode)
import Sound.OSC.FD
import Sound.SC3.FD

import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as M

import Sound.SC3.Lepton.Pattern

default (Integer, Double)

------------------------------------------------------------------------------
-- Command line wrapper utils

-- | Kill running threads in ServerEnv.
shutdownServer :: MVar ServerEnv -> IO ()
shutdownServer mvar =
  F.mapM_ (F.mapM_ killThread . getThreadId) . envThreads =<< readMVar mvar

-- | Wrapper to hold command line argument.
data LeptSeq = LeptSeq
  { port :: Int
  , sc :: (String,Int,Protocol)
  } deriving (Eq,Show,Data,Typeable)

-- | Default setting for command line argument.
defaultLeptSeq :: LeptSeq
defaultLeptSeq = LeptSeq 58110 ("127.0.0.1",57110,Udp)

-- | Make initial env from command line argument.
mkInitEnv :: LeptSeq -> IO (MVar ServerEnv)
mkInitEnv lep = do
  let (h,p,ptc) = sc lep
  newMVar $ ServerEnv M.empty (ConInfo h p ptc) undefined (port lep)

------------------------------------------------------------------------------
-- Types

-- | Protocol type.
data Protocol = Tcp | Udp deriving (Eq,Show,Data,Typeable)

-- | Con information used for scsynth server.
data ConInfo = ConInfo
  { ciHost :: String
  , ciPort :: Int
  , ciProtocol :: Protocol
  } deriving (Eq, Show)

-- | Wrapper for Transport class.
data Con = UDPCon UDP | TCPCon TCP

instance Transport Con where
  sendOSC (UDPCon c) o = sendOSC c o
  sendOSC (TCPCon c) o = sendOSC c o
  recvPacket (UDPCon c) = recvPacket c
  recvPacket (TCPCon c) = recvPacket c
  close (UDPCon c) = close c
  close (TCPCon c) = close c

-- | Environment of server.
data ServerEnv = ServerEnv
  { envThreads :: M.Map String Thread
  , envSC :: ConInfo
  , envLept :: Con
  , envPort :: Int }

-- | Forked thread in server.
data Thread = Thread
  { -- | Status of this thread.
    tState :: ThreadState
    -- | Pattern used by this thread.
  -- , tPat :: R (ToOSC Double)
  , tPat :: L () (ToOSC Double)
    -- | Lock used for pausing thread.
  , tLock :: MVar Double
  } deriving (Eq)

-- | Status for threads forked by server.
data ThreadState
  -- | Pattern is running.
  = Running ThreadId
  -- | Pattern is paused.
  | Paused ThreadId
  -- | Pattern haven't started yet.
  | New
  -- | Pattern has finished running (for finite patterns only).
  | Finished
  deriving (Eq, Show, Ord)

-- | Loop for server.
--
-- Wrapped ReaderT IO with environment in MVar.
newtype ServerLoop a =
  ServerLoop {unServerLoop :: ReaderT (MVar ServerEnv) IO a}
  deriving (Applicative,Functor,Monad,MonadReader (MVar ServerEnv),MonadIO)

-- | Get Con from Coninfo.
fromConInfo :: ConInfo -> IO Con
fromConInfo (ConInfo h p ptc) = case ptc of
  Udp -> UDPCon <$> openUDP h p
  Tcp -> TCPCon <$> openTCP h p

-- | Run server with given env.
runServer :: MVar ServerEnv -> IO ()
runServer env = do
  env' <- readMVar env
  withTransport (udpServer "127.0.0.1" (envPort env')) $ \fd -> do
    modifyMVar_ env $ \env'' -> return $ env'' {envLept=UDPCon fd}
    runReaderT (unServerLoop $ forever work) env

-- | Guts of server.
--
-- Receive OSC message, convert it, and manage patterns.
work :: ServerLoop ()
work = do
  st <- liftIO . readMVar =<< ask
  let lep = envLept st
  msg <- liftIO $ recvPacket lep
  case msg of
    Packet_Bundle (Bundle _ [])     -> return ()
    Packet_Bundle (Bundle time ms)  -> mapM_ (sendMsg (Just time)) ms
    Packet_Message (m@(Message _ _)) -> sendMsg Nothing m

-- | Send message with or without bundled time, and update ServerEnv.
--
-- When the server received `/l_new` message, a new ThreadId will be
-- added. On receiving `/l_free` and `/l_freeAll`, ThreadId of given
-- key will be deleted.
--
-- Timestamp in nested bundle will be ignored, outermost bundle time will
-- be used in whole message.
--
sendMsg :: Maybe Time -> Message -> ServerLoop ()
sendMsg time m = case m of
  -- Bundle _ ms -> mapM_ (sendMsg time) ms
  Message "/l_new" [ASCII_String key, Blob pat] -> runLNew time (unpack key) pat
  Message "/l_free" [ASCII_String key]          -> runLFree time (unpack key)
  Message "/l_freeAll" []                 -> runLFreeAll time
  Message "/l_dump" []                    -> runLDump time
  Message "/l_add" [ASCII_String key, Blob pat] -> runLAdd time (unpack key) pat
  Message "/l_run" [ASCII_String key]           -> runLRun time (unpack key)
  Message "/l_pause" [ASCII_String key]         -> runLPause time (unpack key)
  Message "/l_update" [ASCII_String key, Blob pat] -> runLUpdate time (unpack key) pat
  _ -> do
    st <- liftIO . readMVar =<< ask
    liftIO $ withTransport (fromConInfo (envSC st)) (flip send m)

runLNew :: Maybe Time -> String -> BL.ByteString -> ServerLoop ()
runLNew time key pat = withEnv $ \env ->
  case M.lookup key (envThreads env) of
    Just _  -> liftIO $ putStrLn $ "thread exists: " ++ key
    Nothing -> do
      case decodePattern pat of
        Right r  -> forkNewThread time key r
        Left err -> liftIO $ putStrLn err

runLAdd :: Maybe Time -> String -> BL.ByteString -> ServerLoop ()
runLAdd _ key pat = case decodePattern pat of
  Right pat' -> modifyEnv $ \env -> do
    lck <- liftIO $ newEmptyMVar
    let t = Thread New pat' lck
    return $ env {envThreads=M.insert key t (envThreads env)}
  Left err   -> liftIO $ putStr err

runLRun :: Maybe Time -> String -> ServerLoop ()
runLRun time0 key = withEnv $ \env ->
  case M.lookup key (envThreads env) of
    Just (Thread New pat _)        -> forkNewThread time0 key pat
    Just (Thread Finished pat _)   -> forkNewThread time0 key pat
    Just (Thread (Paused i) pat lck) -> modifyEnv $ \env' -> do
      time' <- liftIO $ maybe time (return) time0
      liftIO $ putMVar lck time'
      let t = Thread (Running i) pat lck
      return $ env' {envThreads = M.adjust (const t) key (envThreads env')}
    _                              -> return ()

runLPause :: Maybe Time -> String -> ServerLoop ()
runLPause time key = modifyEnv $ \env ->
  case M.lookup key (envThreads env) of
    Just (Thread (Running i) p lck) ->  do
      liftIO (maybePause time >> takeMVar lck >> return ())
      let t = Thread (Paused i) p lck
      return $ env {envThreads = M.adjust (const t) key (envThreads env)}
    _ -> return env

runLUpdate :: Maybe Time -> String -> BL.ByteString -> ServerLoop ()
runLUpdate time0 key pat = do
  case decodePattern pat of
    Right pat' -> do
      mvar <- ask
      liftIO $ modifyMVar_ mvar $ \env -> do
        let tmap = envThreads env
        env' <- case M.lookup key tmap of
          Just t -> do
            maybePause time0
            F.mapM_ killThread (getThreadId t)
            return $ env {envThreads=M.delete key tmap}
          Nothing -> return env
        lck <- newEmptyMVar
        tid <- forkIO $ withTransport (fromConInfo $ envSC env) $ \fd ->
          bracket
            (do send fd (notify True)
                now <- time
                time' <- maybe (return now)
                  (\t -> return $ if now > t then now else t) time0
                trid <- newNid
                return (time',trid,fd))
            (\(_,trid,fd') -> do
                 -- modifyMVar_ mvar $ \env' ->
                 --   return $ env' {envThreads=M.delete key (envThreads env')}
                 sendOSC fd' $ bundle immediately [notify False, n_free [trid]])
            (\(time',trid,fd') -> do
                -- putMVar lck (as_utcr time')
                -- runPausableMsg lck pat' trid fd')
                withNotifications fd' (runReaderT (runMsgFrom time' pat' trid)))
        let t = Thread (Running tid) pat' lck
            env'' = env' {envThreads=M.insert key t (envThreads env')}
        -- liftIO $ print (fmap showThread $ envThreads env'')
        return $ env' {envThreads=M.insert key t (envThreads env')}
    Left err   -> liftIO $ putStr err

{-
forkNewThread :: Maybe Time -> String -> R (ToOSC Double) -> ServerLoop ()
forkNewThread time key pat = do
  mvar <- ask
  modifyEnv $ \env -> do
    lck <- liftIO $ newMVar ()
    tid <- liftIO $ forkIO $ withTransport (fromConInfo $ envSC env) $ \fd ->
      bracket
        (do send fd (notify True)
            now <- utcr
            time' <- maybe (return $ UTCr now)
              (\t -> return $ if UTCr now > t then (UTCr now) else t) time
            trid <- newNid
            return (time',trid,fd))
        (\(_,trid,fd') -> do
            let f (Thread _ p l) = Thread Finished p l
            modifyMVar_ mvar $ \env' -> do
              return $ env' {envThreads=M.adjust f key (envThreads env')}
            send fd' $ bundle immediately [notify False, n_free [trid]])
        (\(time',trid,fd') -> runMsgFrom time' pat trid fd')
    let t = Thread (Running tid) pat lck
    return $ env {envThreads=M.insert key t (envThreads env)}
-}

forkNewThread :: Maybe Time -> String -> L () (ToOSC Double) -> ServerLoop ()
forkNewThread time0 key pat = do
  mvar <- ask
  modifyEnv $ \env -> do
    lck <- liftIO newEmptyMVar
    tid <- liftIO $ forkIO $ withTransport (fromConInfo $ envSC env) $ \fd ->
      bracket
        (do send fd (notify True)
            now <- time
            time' <- maybe (return now)
              (\t -> return $ if now > t then now else t) time0
            trid <- newNid
            return (time',trid,fd))
        (\(_,trid,fd') -> do
            let f (Thread _ p l) = Thread Finished p l
            modifyMVar_ mvar $ \env' -> do
              -- return $ env' {envThreads=M.adjust f key (envThreads env')}
              return $ env' {envThreads=M.delete key (envThreads env')}
            sendOSC fd' $ bundle immediately [notify False, n_free [trid]])
        (\(time',trid,fd') -> do
            putMVar lck (time')
            runReaderT (runPausableMsg lck pat trid) fd')
    let t = Thread (Running tid) pat lck
    return $ env {envThreads=M.insert key t (envThreads env)}

runLFree :: Maybe Time -> String -> ServerLoop ()
runLFree time key = modifyEnv $ \env ->
  let tmap = envThreads env
  in  case M.lookup key tmap of
    Just t -> do
      liftIO $ forkIO $ do
        maybePause time
        F.mapM_ killThread (getThreadId t)
      return $ env {envThreads=M.delete key tmap}
    Nothing -> return env

runLFreeAll :: Maybe Time -> ServerLoop ()
runLFreeAll time = modifyEnv $ \env -> do
  maybePause time
  F.mapM_ (F.mapM_ killThread . getThreadId) (envThreads env)
  return $ env {envThreads=M.empty}

runLDump :: Maybe Time -> ServerLoop ()
runLDump time = withEnv $ \env -> do
  liftIO $ forkIO $ do
    maybePause time
    putStrLn "Threads: "
    mapM_ (\(k,t) -> putStrLn $ "  " ++ k ++ ":" ++ showThread t) $
      L.sortBy (comparing (tState . snd)) $ M.toList $ envThreads env
  return ()

------------------------------------------------------------------------------
-- Utils

-- | Modify env with given action.
modifyEnv :: (ServerEnv -> IO ServerEnv) -> ServerLoop ()
modifyEnv k = ask >>= \mvar -> liftIO $ modifyMVar_ mvar k

-- | Execute given action with current env.
withEnv :: (ServerEnv -> ServerLoop a) -> ServerLoop a
withEnv k = ask >>= liftIO . readMVar >>= k

-- withEnv_ :: (Env -> ServerLoop a) -> ServerLoop ()
-- withEnv_ k = withEnv (\e -> k e >> return ())

-- | Pause until given 'Time', or return immediately on 'Nothing'.
maybePause :: Maybe Time -> IO ()
maybePause = F.mapM_ $ \time0 -> do
  dt <- (time0 -) <$> time
  let (q,_) = properFraction (dt * 1e6)
  when (dt > 0) $ threadDelay q

-- | Show thread, used in dumped message.
showThread :: Thread -> String
showThread (Thread st _ _) = show st

-- | Return ThreadId when given thread is running or pause, otherwise Nothing.
getThreadId :: Thread -> Maybe ThreadId
getThreadId (Thread st _ _) = case st of
  Running tid -> Just tid
  Paused tid  -> Just tid
  _           -> Nothing

decodePattern :: BL.ByteString -> Either String (L () (ToOSC Double))
decodePattern = t2l . decode . Z.decompress

{-
decodePattern :: BL.ByteString -> Either String (R (ToOSC Double))
decodePattern = fmap toR . parseP . Z.decompress

decodePattern pat = toR <$> fromExpr (decode $ Z.decompress pat)
-}
