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
module Scratch.Server where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.State
import Data.Data

import Data.Binary (decode)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import System.Console.CmdArgs (cmdArgs)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H

-- ---------------------------------------------------------------------------
-- Command line wrapper

main :: IO ()
main = bracket acquire release server where
  acquire = do
    opt <- cmdArgs defaultArg
    env <- mkInitEnv opt
    return env
  release env = return env

data Lepton = Lepton
  { port :: Int
  , sc :: (String,Int,Protocol)
  } deriving (Eq,Show,Data,Typeable)

defaultArg :: Lepton
defaultArg = Lepton 58110 ("127.0.0.1",57110,Udp)

serveTest :: IO ()
serveTest = bracket acquire release server where
  acquire = do
    env <- mkInitEnv defaultArg
    return env
  release = return

mkInitEnv :: Lepton -> IO Env
mkInitEnv lep = do
  let (h,p,ptc) = sc lep
  return $ Env H.empty (ConInfo h p ptc) undefined (port lep)

{-
TODO:

* Add pause, resume command.

* Kill threads when server has been killed, in ghci.

* Remove itself from Map when finite threads has ended.
-}

-- ---------------------------------------------------------------------------
-- Types

data Protocol = Tcp | Udp deriving (Eq,Show,Data,Typeable)

data ConInfo = ConInfo
  { ciHost :: String
  , ciPort :: Int
  , ciProtocol :: Protocol
  } deriving (Eq, Show)

data Connection = PUDP UDP | PTCP TCP

instance Transport Connection where
  send (PUDP c) o = send c o
  send (PTCP c) o = send c o
  recv (PUDP c) = recv c
  recv (PTCP c) = recv c
  close (PUDP c) = close c
  close (PTCP c) = close c

data Env = Env
  { envThreads :: H.HashMap String ThreadId
  , envSC :: ConInfo
  , envLept :: Connection
  , envPort :: Int }

-- | Pattern server loop. Wrapping StateT inside it.
newtype ServerLoop a = ServerLoop {unServerLoop :: StateT Env IO a}
  deriving (Applicative,Functor,Monad,MonadState Env, MonadIO)

fromConInfo :: ConInfo -> IO Connection
fromConInfo (ConInfo h p ptc) = case ptc of
  Udp -> PUDP <$> openUDP h p
  Tcp -> PTCP <$> openTCP h p

server :: Env -> IO ()
server env = withTransport (udpServer "127.0.0.1" (envPort env)) $ \fd ->
  evalStateT (unServerLoop $ forever work) (env {envLept = PUDP fd})

-- | Guts of server.
--
-- Receive OSC message, convert it, and manage patterns.
work :: ServerLoop ()
work = do
  st <- get
  let lep = envLept st
  msg <- liftIO $ recv lep
  case msg of
    Bundle _ []     -> return ()
    Bundle time ms  -> mapM_ (sendMessage (Just time)) ms
    m@(Message _ _) -> sendMessage Nothing m

-- | Send message with or without bundled time, and update Env.
--
-- When the server received @/l_new@ message, a new ThreadId will be
-- added. On receiving @/l_free@ and @/l_freeAll@, ThreadId of given
-- key will be deleted.
--
-- Timestamp in nested bundle will be ignored, outermost bundle time will
-- be used in whole message.
sendMessage :: Maybe Time -> OSC -> ServerLoop ()
sendMessage time m = case m of
  Bundle _ ms -> mapM_ (sendMessage time) ms
  Message "/l_new" [String key, Blob pat] -> runLNew time key pat
  Message "/l_free" [String key]          -> runLFree time key
  Message "/l_freeAll" []                 -> runLFreeAll time
  Message "/l_dump" []                    -> runLDump time
  _ -> do
    st <- get
    liftIO $ withTransport (fromConInfo (envSC st)) (\fd -> send fd m)

runLNew :: Maybe Time -> String -> BL.ByteString -> ServerLoop ()
runLNew time key pat = do
  st <- get
  case H.lookup key (envThreads st) of
    Nothing -> do
      case fromExpr (decode pat) of
        Right (pat'::R (ToOSC Double)) -> do
          st <- get
          let sc = envSC st
          tid <- liftIO $ forkIO $ withTransport (fromConInfo sc) $ \fd ->
            bracket
              (do send fd (notify True)
                  time' <- maybe (UTCr <$> utcr) return time
                  trid <- newNid
                  return (time',trid,fd))
              (\(_,trid,fd') ->
                send fd' (bundle immediately [notify False, n_free [trid]]))
              (\(time',trid,fd') ->
                runMsgFrom time' pat' trid fd')
          modify (\st -> st {envThreads = H.insert key tid (envThreads st)})
        Left err -> liftIO $ putStrLn err
    Just _ -> liftIO $ putStrLn $ "thread exists: " ++ key

runLFree :: Maybe Time -> String -> ServerLoop ()
runLFree time key = do
  st <- get
  let tm = envThreads st
  tm' <- case H.lookup key tm of
    Just tid -> do
      liftIO $ forkIO $ do
        maybePause time
        killThread tid
      return $ H.delete key tm
    Nothing -> return tm
  modify (\st -> st {envThreads = tm'})

runLFreeAll :: Maybe Time -> ServerLoop ()
runLFreeAll time = do
  st <- get
  liftIO $ forkIO $ do
    maybePause time
    F.mapM_ killThread $ envThreads st
  modify (\st -> st {envThreads = H.empty})

runLDump :: Maybe Time -> ServerLoop ()
runLDump time = do
  st <- get
  liftIO $ forkIO $ do
    maybePause time
    mapM_ print $ H.toList $ envThreads st
  return ()

-- | Pause until given 'Time', or return immediately on 'Nothing'.
maybePause :: Maybe Time -> IO ()
maybePause = F.mapM_ $ \time -> do
  dt <- (as_utcr time -) <$> utcr
  when (dt > 0) $ threadDelay (ceiling $ dt*10^6)