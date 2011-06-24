{-# LANGUAGE ScopedTypeVariables #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with TCP network client, from:
--
-- * http://sequence.complete.org/node/257
--
module TCPClient where

import Prelude hiding (catch)

import Network (connectTo, withSocketsDo, PortID(..))
import System.IO
import System.IO.Error (isEOFError)
import System.Environment (getArgs)
import Control.Exception (IOException(..), finally, catch)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)

main :: IO ()
main = withSocketsDo $ do
  sock <- connectTo "127.0.0.1" (PortNumber 1503)
  hSetBuffering sock LineBuffering
  start sock `catch` handler `finally` hClose sock
  where
    handler :: IOException -> IO ()
    handler e | isEOFError e = return ()
              | otherwise    = print e

start :: Handle -> IO ()
start sock = do
  netChan <- atomically newTChan
  userChan <- atomically newTChan
  spawn $ listenLoop (hGetLine sock) netChan
  spawn $ listenLoop getLine userChan
  mainLoop sock netChan userChan

spawn :: IO () -> IO ThreadId
spawn act = do
  mainTID <- myThreadId
  forkIO $ act `catch` (throwTo mainTID :: IOException -> IO ())

listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan = forever $ act >>= atomically . writeTChan chan

mainLoop :: Handle -> TChan String -> TChan String -> IO ()
mainLoop sock netChan userChan = forever $ do
  input <- atomically $ select netChan userChan
  case input of
    Left str  -> putStrLn str >> hFlush stdout
    Right str -> hPutStrLn sock str >> hFlush sock

select :: TChan a -> TChan b -> STM (Either a b)
select ch1 ch2 = leftChan `orElse` rightChan where
  leftChan  = readTChan ch1 >>= return . Left
  rightChan = readTChan ch2 >>= return . Right