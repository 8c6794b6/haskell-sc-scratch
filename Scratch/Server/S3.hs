------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to ghc)
--
-- Playing with concurrency, take 3.
--
-- Using network related function and actions from OpenSoundControl module.
-- Server will respond to OSC message.
--
module S3 where

import Control.Monad
import Control.Concurrent
import Control.Exception (bracket)
import Data.Map (Map)
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3

------------------------------------------------------------------------------
--
-- Server side
--
------------------------------------------------------------------------------

type Act = MVar () -> IO ()

type ServerEnv = MVar (Map String ThreadInfo)

data ThreadInfo = ThreadInfo
  { tiId :: ThreadId
  , tiStatus :: ChildStatus
  , tiBlock :: MVar () }

data ChildStatus = Running
                 | Paused
                   deriving (Eq, Show)

serve :: String -> Int -> [(String,Act)] -> IO ()
serve host port acts = bracket (udpServer host port) close $ \con -> do
  putStrLn $ "Server stated, host:" ++ host ++ ", port:" ++ show port
  e <- initialEnv
  initialize e acts
  forever $ do
    msg <- recv con
    respondMsg e msg

-- | Serve with localhost, UDP port 57130.
serveDefault :: [(String,Act)] -> IO ()
serveDefault = serve "127.0.0.1" 57130

initialEnv :: IO ServerEnv
initialEnv = newMVar M.empty

initialize :: ServerEnv -> [(String, Act)] -> IO ()
initialize evar ts = do
  env <- readMVar evar
  tis <- mapM (uncurry forkChild) ts
  modifyMVar_ evar (\m -> return $ foldr (uncurry M.insert) m tis)

forkChild :: String -> Act -> IO (String, ThreadInfo)
forkChild name act = do
  blk <- newMVar ()
  tid <- forkIO $ act blk
  return $ (name, ThreadInfo tid Running blk)

respondMsg :: ServerEnv -> OSC -> IO ()
respondMsg e msg = case msg of
  Message "/pause" [String name]  -> pauseChild e name
  Message "/resume" [String name] -> resumeChild e name
  Message "/kill" [String name]   -> killChild e name
  Message "/add" _                -> putStrLn $ "add not yet"
  Message "/dump" _               -> dumpEnv e
  Message "/show" _               -> putStrLn $ "show not yet"
  _                               -> putStrLn $ "Unknown message: " ++ show msg

dumpEnv :: ServerEnv -> IO ()
dumpEnv evar = do
  e <- readMVar evar
  forM_ (M.assocs e) $ \(n,ti) -> do
    putStr $ n ++ "\t"
    putStrLn $ show (tiId ti) ++ ", Status: " ++ show (tiStatus ti)
  putStr "\n"

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
    Just (ThreadInfo _ Paused _) -> return ()
    Just (ThreadInfo _ Running mv) -> do
      takeMVar mv
      modifyMVar_ evar $
        return . M.update (\ti -> return $ ti {tiStatus = Paused}) name
    Nothing -> putStrLn $ "No action found: " ++ name

resumeChild :: ServerEnv -> String -> IO ()
resumeChild evar name = do
  e <- readMVar evar
  case M.lookup name e of
    Just (ThreadInfo _ Paused mv) -> do
      putMVar mv ()
      modifyMVar_ evar $
        return . M.update (\ti -> return $ ti {tiStatus = Running}) name
    Just (ThreadInfo _ Running _) -> return ()
    Nothing -> putStrLn $ "No action found: " ++ name

------------------------------------------------------------------------------
--
-- Client side
--
------------------------------------------------------------------------------

withL :: (UDP -> IO a) -> IO a
withL = bracket (openUDP "127.0.0.1" 57130) close

killAct :: String -> OSC
killAct name = Message "/kill" [String name]

dumpActs :: OSC
dumpActs = Message "/dump" []

pauseAct :: String -> OSC
pauseAct name = Message "/pause" [String name]

resumeAct :: String -> OSC
resumeAct name = Message "/resume" [String name]
