------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to ghc)
--
-- Playing with concurrency, server side, take 4.
--
-- Using network related function and actions from OpenSoundControl module.
-- Server will respond to OSC message.
-- 
-- 
module S4 where

import Control.Monad
import Control.Concurrent
import Control.Exception (bracket)
import Data.Map (Map)
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3

type ServerEnv = MVar (Map String ThreadInfo)

data ThreadInfo = ThreadInfo
  { tiId :: ThreadId
  , tiStatus :: ChildStatus
  , tiBlock :: MVar () }
  
instance Show ThreadInfo where
  show ti = show (tiId ti) ++ ", " ++ "Status: " ++ show (tiStatus ti)

data ChildStatus = Running
                 | Paused
                   deriving (Eq, Show)

serve :: String -> Int -> [(String,IO ())] -> IO ()
serve host port acts = bracket (udpServer host port) close $ \con -> do
  putStrLn $ "Server stated, host:" ++ host ++ ", port:" ++ show port
  e <- initialEnv
  initialize e acts
  forever $ recv con >>= respondMsg e

-- | Serve with localhost, UDP port 57130.
serveDefault :: [(String,IO ())] -> IO ()
serveDefault = serve "127.0.0.1" 57130

initialEnv :: IO ServerEnv
initialEnv = newMVar M.empty

initialize :: ServerEnv -> [(String, IO ())] -> IO ()
initialize evar ts = do
  env <- readMVar evar
  tis <- mapM (uncurry forkChild) ts
  modifyMVar_ evar (\m -> return $ foldr (uncurry M.insert) m tis)

forkChild :: String -> IO () -> IO (String, ThreadInfo)
forkChild name act = do
  blk <- newMVar ()
  tid <- forkIO $ forever (readMVar blk >> act)
  return $ (name, ThreadInfo tid Running blk)

respondMsg :: ServerEnv -> OSC -> IO ()
respondMsg e msg = case msg of
  Message "/pause" [String name]  -> pauseChild e name
  Message "/resume" [String name] -> resumeChild e name
  Message "/kill" [String name]   -> killChild e name
  Message "/add" _                -> putStrLn $ "add not yet"
  Message "/dump" _               -> dumpEnv e
  Message "/show" [String name]   -> showChild e name
  _                               -> putStrLn $ "Unknown message: " ++ show msg

dumpEnv :: ServerEnv -> IO ()
dumpEnv evar = do
  e <- readMVar evar
  forM_ (M.assocs e) $ \(n,ti) -> putStrLn $ n ++ ":\t" ++ show ti
  putStr "\n"
  
showChild :: ServerEnv -> String -> IO ()  
showChild evar name = do
  putStr (name ++ ":\t")
  readMVar evar >>=
    maybe (putStrLn $ "Not found") print . M.lookup name

killChild :: ServerEnv -> String -> IO ()
killChild evar name = do
  e <- readMVar evar
  case M.lookup name e of
    Just (ThreadInfo tid _ _) ->
      killThread tid >> modifyMVar_ evar (return . M.delete name)
    Nothing -> putStr $ "No such thead to kill: " ++ name

pauseChild :: ServerEnv -> String -> IO ()
pauseChild evar name = do
  e <- readMVar evar
  case M.lookup name e of
    Just (ThreadInfo _ Paused _)   -> return ()
    Just (ThreadInfo _ Running mv) -> do
      takeMVar mv
      modifyMVar_ evar $
        return . M.update (\ti -> return $ ti {tiStatus = Paused}) name
    Nothing                        -> putStrLn $ "No such action: " ++ name

resumeChild :: ServerEnv -> String -> IO ()
resumeChild evar name = do
  e <- readMVar evar
  case M.lookup name e of
    Just (ThreadInfo _ Paused mv) -> do
      putMVar mv ()
      modifyMVar_ evar $
        return . M.update (\ti -> return $ ti {tiStatus = Running}) name
    Just (ThreadInfo _ Running _) -> return ()
    Nothing                       -> putStrLn $ "No such action: " ++ name