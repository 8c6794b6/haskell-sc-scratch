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

import Data.Serialize (encode, decode)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

-- For sample data
import S6 (pspe, psw, loop01, loop02, loop03)

main :: IO ()
main = server 58110

withLept :: (UDP -> IO a) -> IO a
withLept = withTransport (openUDP "127.0.0.1" 58110)

server :: Int -> IO ()
server port =
  bracket (udpServer "127.0.0.1" port) close $ loop . initialEnv . PUDP

data Env = Env
  { envThreads :: M.Map String ThreadId
  , envSC :: IO Connection
  , envLept :: IO Connection }

data Connection = PUDP UDP | PTCP TCP

instance Transport Connection where
  send (PUDP c) o = send c o
  send (PTCP c) o = send c o
  recv (PUDP c) = recv c
  recv (PTCP c) = recv c
  close (PUDP c) = close c
  close (PTCP c) = close c

initialEnv :: Connection -> Env
initialEnv lep = Env M.empty (PUDP `fmap` openUDP "127.0.0.1" 57110) (return lep)

loop :: Env -> IO ()
loop env = envSC env >>= \sc -> envLept env >>= \lep -> go sc lep where
  go sc fd = do
    m <- recv fd
    case m of
      Message "/l_new" [String key,Blob pat] -> do
        let Right pat' = decode $ B.concat $ BL.toChunks pat
            Right pat'' = fromExpr pat'
        tid <- forkIO $ play sc (pat'' :: R (ToOSC Double))
        let threads = M.insert key tid (envThreads env)
        putStrLn $ "New pattern: " ++ key
        loop (env {envThreads = threads})
      Message "/l_free" [String key] -> do
        case M.lookup key (envThreads env) of
          Just tid -> killThread tid >> putStrLn ("Freed: " ++ key)
          Nothing  -> putStrLn $ "Thread " ++ key ++ " not found"
        loop (env {envThreads = M.delete key (envThreads env)})
      Message "/l_dump" _ -> do
        putStrLn $ M.showTree $ envThreads env
        loop env
      _ -> loop env

-- ---------------------------------------------------------------------------
-- OSC Messages building functions

l_new :: String -> Expr (ToOSC Double) -> OSC
l_new key pat = Message "/l_new" [String key,Blob pat'] where
  pat' = BL.fromChunks $ [encode pat]

l_free :: String -> OSC
l_free key = Message "/l_free" [String key]

l_dump :: OSC
l_dump = Message "/l_dump" []
