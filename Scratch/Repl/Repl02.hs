{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with Shellac package.
--
module Repl02 where

import Control.Concurrent (forkIO, throwTo, threadDelay, myThreadId)
import Control.Exception (bracket, IOException)
import Control.Monad (forever, forM_)
import Network (PortID(..), connectTo)
import Network.Socket 
import System.IO 
import System.IO.Error
import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C8

import qualified Sound.OpenSoundControl as O

-- main :: IO ()
-- main = do 
--   n <- runShell (mkShellDescription cmds work) haskelineBackend 0
--   putStrLn $ "You've entered " ++ show n ++ " line" ++ 
--     (if n > 1 then "s" else "") ++ "."
--   putStrLn $ "Bye!"
    
-- work :: String -> Sh Int ()
-- work [] = return ()
-- work x  = modifyShellSt (+1) >> shellPutStrLn x

main :: IO ()
main = ls

ls :: IO ()
-- ls = bracket
--   (connectTo "127.0.0.1" (PortNumber 1503)) 
--   hClose $ \h -> do
--     hSetBuffering h NoBuffering
--     hPutStrLn h "ls"
--     hFlush h
--     hGetContents h >>= putStrLn
--     hClose h

-- ls = withSocketsDo $ do
--   h <- connectTo "127.0.0.1" (PortNumber 1503)
--   hSetBuffering h LineBuffering
--   hPutStr h "ls\n\n"
--   hPutStr h "quit\n\n"
--   hFlush h
--   contents <- hGetContents h
--   putStrLn contents
--   -- hPutStr h "quit\n\n"
--   hClose h 

-- ls = withSocketsDo $ do
--   h <- connectTo "127.0.0.1" (PortNumber 1503)
--   hSetBuffering h NoBuffering
--   hPutStr h "quit\n"
--   contents <- hGetContents h
--   putStrLn contents
--   hClose h

-- ls = let con = connectTo "127.0.0.1" (PortNumber 1503)
--      in withSocketsDo $ bracket con hClose $ \h -> do
--        hSetBuffering h NoBuffering
--        hGetLine h >>= putStr >> hFlush stdout

--        hPutStr h "ls\n"
--        -- contents <- hGetContents h
       
--        -- putStr contents >> hFlush stdout
--        hGetLine h >>= putStr >> hFlush stdout
--        -- threadDelay (1000000)
--        putStrLn "sending quit"
--        hFlush stdout
--        hPutStr h "quit\n"
--        putStrLn "sent quit"
--        hFlush h
--        contents <- hGetContents h
--        putStr contents >> hFlush stdout
--        hFlush stdout

ls = withSocketsDo $ do
  bracket (connectTo "127.0.0.1" (PortNumber 1503)) hClose $ \hd -> do
    hSetBuffering hd LineBuffering
    let reader = hGetLine hd >>= putStrLn >> hFlush stdout >> hFlush hd
        cmd str = do
          hPutStrLn hd str
          hFlush hd
    mainId <- myThreadId
    forkIO $ forever reader `catch` throwTo mainId
    forever $ getLine >>= cmd
       
-- ls = do
--   putStrLn "Getting addrInfo"
--   addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "1503")
--   let serveraddr = head $ filter ((== AF_INET) . addrFamily) addrinfos
--   sock <- socket AF_INET Stream defaultProtocol
--   setSocketOption sock KeepAlive 1
--   putStrLn "Connecting socket"
--   print $ addrAddress serveraddr
--   connect sock (addrAddress serveraddr)
--   bracket (socketToHandle sock ReadWriteMode) hClose $ \h -> do
--     hSetBuffering h LineBuffering
--     forM_ [1..3] $ \_ -> do
--       hPutStrLn h "ls"
--       hFlush h
--       hGetContents h >>= putStrLn
--       threadDelay (100000*3)
    
  -- h <- socketToHandle sock ReadWriteMode
  -- hSetBuffering h (BlockBuffering Nothing)
  -- hPutStrLn h "ls"
  -- hFlush h
  -- return ()
