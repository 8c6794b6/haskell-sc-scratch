------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with concurrency.
--
module S1 where

import Control.Concurrent
import Control.Monad
import Data.List (isPrefixOf)
import Data.Map (Map)

import qualified Data.Map as M
import qualified Network.Socket as N

main :: IO ()
main = serve $ show port

host :: String
host = "127.0.0.1"

port :: Int
port = 57130

type ServerEnv = MVar (Map String Int)

initialEnv :: IO ServerEnv
initialEnv = newMVar M.empty

-- | This server is responding to message.
--
-- Though, it seems like a goal so far beyond this to make a general
-- purpose message responding server for scheduling threads.
--
serve :: String -> IO ()
serve port = N.withSocketsDo $ do
  addrinfos <- N.getAddrInfo
    (Just (N.defaultHints { N.addrFlags = [N.AI_PASSIVE]})) Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
  N.bindSocket sock (N.addrAddress serveraddr)
  e <- initialEnv
  forever $ procMessages e sock
  where
    procMessages e sock = do
      msg <- N.recv sock 1024
      work msg e

work :: String -> ServerEnv -> IO ()
work msg mvar
  | "insert" `isPrefixOf` msg = do
    let (_:k:v:_) = words msg
    putStrLn $ "key: " ++ k ++ ", val: " ++ v
    modifyMVar_ mvar (\m -> return $ M.insert k (read v) m)
    putStrLn $ "done"

  | "p" `isPrefixOf` msg = do
     e <- readMVar mvar
     print e

  | otherwise = do
    putStrLn $ "Got: " ++ msg

client :: Int -> String -> IO ()
client port msg = do
  addrinfos <- N.getAddrInfo Nothing (Just host) (Just $ show port)
  let serveraddr = head addrinfos
  sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
  c <- N.sendTo sock msg (N.addrAddress serveraddr)
  N.sClose sock
  print c

askServer :: Int -> IO String
askServer port = do
  undefined
