{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (ghc concurrency)

Server to manage patterns, take 2.
Writing server with using hosc package. Communication between client would
be done with sending and receiving OSC command, instead of raw ByteString.

-}
module S7 where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Data

import Data.Binary (decode)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import System.Console.CmdArgs (cmdArgs)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.Map as M

main :: IO ()
main = bracket acquire release server where
  acquire = do
    opt <- cmdArgs defaultArg
    env <- mkInitEnv opt
    send (envSC env) (notify True)
    return env
  release env = close (envSC env)

defaultArg = Lepton 58110 ("127.0.0.1",57110,Udp)

serveTest :: IO ()
serveTest = bracket acquire release server where
  acquire = do
    env <- mkInitEnv defaultArg
    send (envSC env) (notify True)
    return env
  release = close . envSC

data Lepton = Lepton
  { port :: Int
  , sc :: (String,Int,Protocol)
  } deriving (Eq,Show,Data,Typeable)

data Protocol = Tcp | Udp deriving (Eq,Show,Data,Typeable)

mkInitEnv :: Lepton -> IO Env
mkInitEnv lep = do
  scCon <- case sc lep of
        (host,port,Udp) -> PUDP <$> openUDP host port
        (host,port,Tcp) -> PTCP <$> openTCP host port
  return $ Env M.empty scCon undefined (port lep)

data Env = Env
  { envThreads :: M.Map String ThreadId
  , envSC :: Connection
  , envLept :: Connection
  , envPort :: Int }

data Connection = PUDP UDP | PTCP TCP

instance Transport Connection where
  send (PUDP c) o = send c o
  send (PTCP c) o = send c o
  recv (PUDP c) = recv c
  recv (PTCP c) = recv c
  close (PUDP c) = close c
  close (PTCP c) = close c

server :: Env -> IO ()
server env =
  bracket (udpServer "127.0.0.1" (envPort env)) close $ \fd -> do
    runServerLoop (env {envLept = PUDP fd})

type ServerLoop a = StateT Env IO a

runServerLoop :: Env -> IO a
runServerLoop env = evalStateT (forever work) env

{-|

Guts of server.

TODO:

* Send bundled osc messages.

* Handle timestamp to yield new threads.

* Handle timestamp to kill forked threads.

* Add pause, resume command.

-}
work :: StateT Env IO ()
work = do
  st <- get
  let lep = envLept st
  msg <- liftIO $ recv lep
  case msg of
    Bundle _ []     -> return ()
    Bundle time ms  -> mapM_ sendMessage ms
    m@(Message _ _) -> sendMessage m

sendMessage :: OSC -> StateT Env IO ()
sendMessage m = case m of
  Message "/l_new" [String key, Blob pat] -> do
    liftIO $ print pat
    case fromExpr (decode pat) of
      Right (pat'::R (ToOSC Double)) -> do
        st <- get
        let sc = envSC st
        trid <- liftIO newNid
        tid <- liftIO $ (forkIO $ runMsg pat' trid sc)
        modify (\st -> st {envThreads = M.insert key tid (envThreads st)})
      Left err -> do
        liftIO $ putStrLn err
  Message "/l_free" [String key] -> do
    st <- get
    let tm = envThreads st
    tm' <- case M.lookup key tm of
      Just tid -> do
        liftIO $ killThread tid
        return $ M.delete key tm
      Nothing -> return tm
    modify (\st -> st {envThreads = tm'})
  Message "/l_freeAll" [] ->
    liftIO . F.mapM_ killThread . envThreads =<< get
  Message "/l_dump" [] -> do
    st <- get
    liftIO $ print $ envThreads st
  _ -> get >>= \st -> liftIO $ send (envSC st) m
  -- _ -> return ()
