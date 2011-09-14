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
  (
    -- * Types
    ConInfo(..)
  , Connection(..)
  , LeptSeq(..)
  , Protocol(..)
  , ServerEnv(..)
  , ServerLoop(..)
  , Thread(..)
  , ThreadState(..)

    -- * Server related actions
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
import Data.Data
import Data.Ord (comparing)

{-
import Data.Binary (Binary, decode)
-}
import Sound.OpenSoundControl
import Sound.SC3 hiding (Binary, env)

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

-- | Connection information used for scsynth server.
data ConInfo = ConInfo
  { ciHost :: String
  , ciPort :: Int
  , ciProtocol :: Protocol
  } deriving (Eq, Show)

-- | Wrapper for Transport class.
data Connection = UDPCon UDP | TCPCon TCP

instance Transport Connection where
  send (UDPCon c) o = send c o
  send (TCPCon c) o = send c o
  recv (UDPCon c) = recv c
  recv (TCPCon c) = recv c
  close (UDPCon c) = close c
  close (TCPCon c) = close c

-- | Environment of server.
data ServerEnv = ServerEnv
  { envThreads :: M.Map String Thread
  , envSC :: ConInfo
  , envLept :: Connection
  , envPort :: Int }

-- | Forked thread in server.
data Thread = Thread
  { -- | Status of this thread.
    tState :: ThreadState
    -- | Pattern used by this thread.
  , tPat :: R (ToOSC Double)
  } deriving (Eq, Show)

-- | Status for threads forked by server.
data ThreadState
  -- | Pattern is running.
  = Running ThreadId
  -- | Pattern is paused.
  | Paused ThreadId
  -- | Pattern haven't started yet.
  | New
  -- | Pattern was finite, finished running it.
  | Finished
  deriving (Eq, Show, Ord)

-- | Loop for server.
--
-- Wrapped ReaderT IO with environment in MVar.
newtype ServerLoop a = ServerLoop {unServerLoop :: ReaderT (MVar ServerEnv) IO a}
  deriving (Applicative,Functor,Monad,MonadReader (MVar ServerEnv),MonadIO)

-- | Get Connection from Coninfo.
fromConInfo :: ConInfo -> IO Connection
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
  msg <- liftIO $ recv lep
  case msg of
    Bundle _ []     -> return ()
    Bundle time ms  -> mapM_ (sendMessage (Just time)) ms
    m@(Message _ _) -> sendMessage Nothing m

-- | Send message with or without bundled time, and update ServerEnv.
--
-- When the server received `/l_new` message, a new ThreadId will be
-- added. On receiving `/l_free` and `/l_freeAll`, ThreadId of given
-- key will be deleted.
--
-- Timestamp in nested bundle will be ignored, outermost bundle time will
-- be used in whole message.
--
sendMessage :: Maybe Time -> OSC -> ServerLoop ()
sendMessage time m = case m of
  Bundle _ ms -> mapM_ (sendMessage time) ms
  Message "/l_new" [String key, Blob pat] -> runLNew time key pat
  Message "/l_free" [String key]          -> runLFree time key
  Message "/l_freeAll" []                 -> runLFreeAll time
  Message "/l_dump" []                    -> runLDump time
  Message "/l_add" [String key, Blob pat] -> runLAdd time key pat
  Message "/l_run" [String key]           -> runLRun time key
  Message "/l_pause"  [String key]        -> runLPause time key
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
    let t = Thread New pat'
    return $ env {envThreads=M.insert key t (envThreads env)}
  Left err   -> liftIO $ putStr err

runLRun :: Maybe Time -> String -> ServerLoop ()
runLRun time key = withEnv $ \env ->
  case M.lookup key (envThreads env) of
    Just (Thread New pat)        -> forkNewThread time key pat
    Just (Thread Finished pat)   -> forkNewThread time key pat
    Just (Thread (Paused _) _)   -> return ()
    _                            -> return ()

-- | XXX: /Not implemented yet/.
runLPause :: Maybe Time -> String -> ServerLoop ()
runLPause _ _ = undefined

forkNewThread :: Maybe Time -> String -> R (ToOSC Double) -> ServerLoop ()
forkNewThread time key pat = do
  mvar <- ask
  modifyEnv $ \env -> do
    tid <- liftIO $ forkIO $ withTransport (fromConInfo $ envSC env) $ \fd ->
      bracket
        (do send fd (notify True)
            now <- utcr
            time' <- maybe (return $ UTCr now)
              (\t -> return $ if UTCr now > t then (UTCr now) else t) time
            trid <- newNid
            return (time',trid,fd))
        (\(_,trid,fd') -> do
            let f (Thread _ p) = Thread Finished p
            modifyMVar_ mvar $ \env' -> do
              return $ env' {envThreads=M.adjust f key (envThreads env')}
            send fd' $ bundle immediately [notify False, n_free [trid]])
        (\(time',trid,fd') -> runMsgFrom time' pat trid fd')
    let t = Thread (Running tid) pat
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
maybePause = F.mapM_ $ \time -> do
  dt <- (as_utcr time -) <$> utcr
  let (q,_) = properFraction (dt * 1e6)
  when (dt > 0) $ threadDelay q

-- | Show thread, used in dumped message.
showThread :: Thread -> String
showThread (Thread st _) = show st

-- | Return ThreadId when given thread is running or pause, otherwise Nothing.
getThreadId :: Thread -> Maybe ThreadId
getThreadId (Thread st _) = case st of
  Running tid -> Just tid
  Paused tid  -> Just tid
  _           -> Nothing

decodePattern :: BL.ByteString -> Either String (R (ToOSC Double))
decodePattern = fmap toR . parseP . Z.decompress

{-
decodePattern pat = toR <$> fromExpr (decode $ Z.decompress pat)
-}
