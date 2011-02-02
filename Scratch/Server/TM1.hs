{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to GHC)
--
-- Playing with concurrency, manager for forked threads, take 1.
-- 
-- /Usage/
-- 
-- 1. Initialize: 
-- 
-- > > e <- initEnv
-- 
-- 2. Write actions:
--
-- > > let action1 = act $ do {  ...  }
-- 
-- 3. Do some work with e and action, e.g. 
-- 
-- > > tadd e "one" action1 
-- > > tpause e "one"
-- > > tresume e "one"
-- 
-- 4. Change TimeUnit
--
-- > > setTimeUnit e 2.5
-- > > tresume e "one"
-- 
module TM1 where

import Control.Concurrent
import Control.Exception
import Control.Monad
import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Reader
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3

data TEnv = TEnv
  { -- teInitTime :: Double? Integral?
    teTimeUnit :: Double
  , teThreads :: M.Map String ThreadInfo }
 
data ThreadInfo = ThreadInfo  
  { tiId :: ThreadId
  , tiStatus :: ThreadStatus
  , tiBlock :: MVar () }  
  
data ThreadStatus = Running | Paused deriving (Eq, Show)

instance Show TEnv where 
  show (TEnv b t) = 
    "TEnv - TimeUnit:" ++ show b ++ ", threads:" ++ (show $ M.size t)
 
initEnv :: IO (MVar TEnv)
initEnv = newMVar (TEnv 1 M.empty)

dumpEnv :: MVar TEnv -> IO ()
dumpEnv tev = do
  te <- readMVar tev
  putStrLn $ show te
  forM_ (M.assocs $ teThreads te) $ \(n,ti) -> do
    putStrLn $ n ++ ": " ++ show (tiId ti) ++ " " ++ show (tiStatus ti)

tadd :: MVar TEnv -> String -> Act () -> IO ()
tadd tev name act = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  if M.member name threads
    then putStrLn (name ++ " already exists.") >> return te
    else do
      ti <- newChild (runAct act tev)
      return $ te {teThreads = M.insert name ti threads}
      
-- | Fork child thread.
newChild :: IO () -> IO ThreadInfo  
newChild act = do
  blk <- newMVar ()  
  tid <- forkIO $ forever $ readMVar blk >> act
  return $ ThreadInfo tid Running blk

tkill :: MVar TEnv -> String -> IO ()
tkill tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> do 
      killThread (tiId ti)
      return $ te {teThreads = M.delete name threads}
    
tpause :: MVar TEnv -> String -> IO ()
tpause tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> case tiStatus ti of
      Paused  -> return te
      Running -> do
        takeMVar (tiBlock ti)
        let f i = return $ i {tiStatus=Paused}
        return $ te {teThreads = M.update f name threads}
      
tresume :: MVar TEnv -> String -> IO ()
tresume tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> case tiStatus ti of
      Running -> return te
      Paused  -> do
        putMVar (tiBlock ti) ()
        let f i = return $ i {tiStatus=Running}
        return $ te {teThreads = M.update f name threads}
  
newtype Act a = Act {unAct :: ReaderT (MVar TEnv) IO a}
              deriving (Monad, MonadIO, MonadReader (MVar TEnv))

runAct :: Act a -> MVar TEnv -> IO a
runAct = runReaderT . unAct

act :: IO () -> Act ()
act = liftIO

getTimeUnit :: Act Double
getTimeUnit = ask >>= \mv -> liftIO (readMVar mv >>= return . teTimeUnit)

setTimeUnit :: MVar TEnv -> Double -> IO ()
setTimeUnit tev tu = modifyMVar_ tev $ \te -> do
  return $ te {teTimeUnit = tu}
  
-- getInitTime :: Act Double
-- getInitTime = ask >>= \mv -> liftIO (readMVar mv >>= return . teInitTime)
