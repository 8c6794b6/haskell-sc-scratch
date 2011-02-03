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
-- 3. Add, pause, resume the thread:
--
-- > > tadd e "one" action1
-- > > tpause e "one"
-- > > tresume e "one"
--
-- 4. Change TimeUnit:
--
-- > > setTimeUnit e 2.5
-- > > tresume e "one"
--
-- 5. Kill thread:
--
-- > > tkill e "one"
--
-- 6. Run multiple threads:
--
-- > > let action2 = do { ... }
-- > > let action3 = do { ... }
-- > > taddAt (bar 1) e "one" action1
-- > > taddAt (bar 1) e "two" action2
-- > > taddAt (bar 2) e "three" action3
--
module TM1 where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Random (randomRIO)
import qualified Data.Map as M

import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer
import Sound.OpenSoundControl
import Sound.SC3

-- | Environment for managing threads.
data TEnv = TEnv
  { teInitTime :: Double                 -- ^ UTCr from OSC.
  , teTimeUnit :: Double                 -- ^ Duration of 1 time unit length.
  , teThreads :: M.Map String ThreadInfo -- ^ Map of ThreadInfo.
  , teTimes :: M.Map ThreadId Double     -- ^ Map of ThreadId and time.
  }

instance Show TEnv where
  show (TEnv _ b t _) =
    "TEnv - TimeUnit:" ++ show b ++ ", threads:" ++ (show $ M.size t)

-- | Data for each threads.
data ThreadInfo = ThreadInfo
  { tiId :: ThreadId
  , tiStatus :: ThreadStatus
  , tiBlock :: MVar ()
  , tiIdealTime :: Double }

data ThreadStatus = Running | Paused deriving (Eq, Show)

type InitialDelay = Double -> Double -> Double

-- | Constantly returns 0.
noDelay :: InitialDelay
noDelay _ _ = 0

-- | Initialize environment for threads.
initEnv :: IO (MVar TEnv)
initEnv = initEnvTU 1

-- | Initialize environment with given TimeUnit.
initEnvTU :: Double -> IO (MVar TEnv)
initEnvTU tu = utcr >>= \t0 -> newMVar (TEnv t0 tu M.empty M.empty)

-- | Dump the contents of thread environment.
dumpEnv :: MVar TEnv -> IO ()
dumpEnv tev = do
  te <- readMVar tev
  putStrLn $ show te
  forM_ (M.assocs $ teThreads te) $ \(n,ti) -> do
    putStrLn $ n ++ ": " ++ show (tiId ti) ++ " " ++ show (tiStatus ti)

-- | Add new action with name to thread environmet.
tadd :: MVar TEnv -> String -> Act () -> IO ()
tadd tev name act = taddAt noDelay tev name act

-- | Add new action with initial delay.
--
-- XXX: Need to adjust initial delay, not working as expected.
--
taddAt :: InitialDelay -> MVar TEnv -> String -> Act () -> IO ()
taddAt f tev name act = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  if M.member name threads
    then putStrLn (name ++ " already exists.") >> return te
    else do
      now <- utcr
      ti <- newChild (runAct act tev) (f now (teTimeUnit te))
      return $ te {teThreads = M.insert name ti threads
                  ,teTimes = M.insert (tiId ti) now (teTimes te) }

-- | Kill action.
tkill :: MVar TEnv -> String -> IO ()
tkill tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> do
      killThread (tiId ti)
      return $ te {teThreads = M.delete name threads}

-- | Pause action.
tpause :: MVar TEnv -> String -> IO ()
tpause tev name = tpauseAt noDelay tev name

-- | Pause action with specifying delay.
tpauseAt :: InitialDelay -> MVar TEnv -> String -> IO ()
tpauseAt f tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> case tiStatus ti of
      Paused  -> return te
      Running -> do
        forkIO $ do
          t0 <- utcr
          threadDelay $ floor $ (latency + f t0 (teTimeUnit te)) * 1e6
          pauseChild ti
        let threads' = M.update (return $ Just ti') name threads
            ti' = ti {tiStatus=Paused}
        return $ te {teThreads = threads'}

-- | Resume action.
tresume :: MVar TEnv -> String -> IO ()
tresume tev name = tresumeAt noDelay tev name

-- | Resume action after specified delay time.
tresumeAt :: InitialDelay -> MVar TEnv -> String -> IO ()
tresumeAt f tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> case tiStatus ti of
      Running -> return te
      Paused  -> do
        forkIO $ do
          t0 <- utcr
          threadDelay $ floor $ (latency + f t0 (teTimeUnit te)) * 1e6
          resumeChild ti
        let ti' = ti {tiStatus=Running}
            threads' = M.update (return $ Just ti') name threads
        return $ te {teThreads = threads'}

-- | Fork child thread.
newChild :: IO () -> Double -> IO ThreadInfo
newChild act del = do
  blk <- newMVar ()
  now <- utcr
  tid <- forkIO $ do
    threadDelay (floor ((latency+del) * 1e6))
    forever $ readMVar blk >> act
  return $ ThreadInfo tid Running blk now

-- | Pause child thread
pauseChild :: ThreadInfo -> IO ()
pauseChild ti = takeMVar (tiBlock ti)

-- | Resume child thread
resumeChild :: ThreadInfo -> IO ()
resumeChild ti = putMVar (tiBlock ti) ()

-- | Action executed in thread managed environment.
newtype Act a = Act {unAct :: ReaderT (MVar TEnv) IO a}
  deriving (Monad, MonadIO, MonadReader (MVar TEnv))

-- | Unwrapper for Act.
runAct :: Act a -> MVar TEnv -> IO a
runAct = runReaderT . unAct

-- | Make @Act@ from @IO@, currently it's a synonym of liftIO.
act :: IO a -> Act a
act = liftIO

-- | Get time unit in thread environment.
getTimeUnit :: Act Double
getTimeUnit = ask >>= \mv -> liftIO (readMVar mv >>= return . teTimeUnit)

-- | Set time unit in thread environment.
setTimeUnit :: MVar TEnv -> Double -> IO ()
setTimeUnit tev tu = modifyMVar_ tev $ \te -> do
  return $ te {teTimeUnit = tu}

-- | Wrapper for threadDelay.
--
-- Updates reference time for each thread in teTimes Map of TEnv.
-- Using pauseThreadUntil from Sound.OpenSoundControl.
--
tdelay :: Double -> Act ()
tdelay dt = when (dt > 1e-4) $ do
  te <- ask
  act $ modifyMVar_ te $ \te' -> do
    mid <- myThreadId
    let tmap = teTimes te'
        told = maybe 0 id . M.lookup mid $ tmap
        tnew = told + dt
    pauseThreadUntil (tnew+latency)
    return $ te' {teTimes = M.update (const $ return tnew) mid tmap}

getInitTime :: Act Double
getInitTime = ask >>= \mv -> liftIO (readMVar mv >>= return . teInitTime)

-- | Get current UTCr.
getNow :: Act Double
getNow = liftIO utcr

-- | Util to get reminder from floating point value division.
grem :: Double -> Double -> Double
grem a b = a - (fromIntegral q  * b)
  where (q,_) = properFraction (a/b)

-- | Wait until the beginning of given modulo of bars.
bars :: Double -> InitialDelay
bars n = (\i u -> (n*u - (i `grem` (n*u))))

-- | Small amount of constant value for every delays occuring in thread
-- management. In seconds.
latency :: Fractional a => a
latency = 0.1
