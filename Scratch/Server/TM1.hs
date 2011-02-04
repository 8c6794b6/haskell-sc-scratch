{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to GHC)
--
-- Simple manager for forked threads.  Main target usecase is for interactive
-- management of audible events, though might be useful for other purposes.
--
-- /Example/
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
-- > > taddAt (atTU 1) e "one" action1
-- > > taddAt (atTU 1) e "two" action2
-- > > taddAt (atTU 2) e "three" action3
--
module TM1 where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.STRef
import System.Random (randomRIO)
import qualified Data.Map as M

import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import Sound.OpenSoundControl
import Sound.SC3

-- | Environment for managing threads.
data TEnv = TEnv
  { -- | UTCr from OSC.
    teInitTime :: Double
    -- | Duration of 1 time unit length.
  , teTimeUnit :: Double
    -- | Map of ThreadInfo.
  , teThreads :: M.Map String ThreadInfo
    -- | Map of ThreadId and last execution time.
  , teTimes :: M.Map ThreadId Double
  }

instance Show TEnv where
  show (TEnv _ b t _) =
    "TEnv - TimeUnit: " ++ show b ++ ", threads: " ++ (show $ M.size t)

-- | Data for each threads.
--
-- Might be better to rewrite with holding the whole @MVar ThreadInfo@ as value
-- of TEnv's teThreads Map. Though, @MVar Double@ referred by tiIdealTime is
-- accessed from each child action threads.
--
data ThreadInfo = ThreadInfo
  { tiId :: ThreadId
  , tiStatus :: ThreadStatus
  , tiBlock :: MVar ()
  , tiIdealTime :: MVar Double
  , tiPausedTime :: MVar Double }

data ThreadStatus = Running | Paused deriving (Eq, Show)

-- | Function to get initial delay with passed arguments.
type InitialDelay
  = Double -- ^ Current time, in UTCr seconds.
 -> Double -- ^ Time Unit
 -> Double -- ^ Delay time in seconds.

-- | Initialize environment for threads with TimeUnit 1.
initEnv :: IO (MVar TEnv)
initEnv = initEnvTU 1

-- | Initialize environment with given TimeUnit.
initEnvTU :: Double -> IO (MVar TEnv)
initEnvTU tu = utcr >>= \t0 -> newMVar (TEnv t0 tu M.empty M.empty)

-- | Dump the contents of thread environment.
dumpEnv :: MVar TEnv -> IO ()
dumpEnv tev = do
  te <- readMVar tev
  putStrLn $ "\n" ++ show te
  forM_ (M.assocs $ teThreads te) $ \(n,ti) -> do
    putStrLn $ n ++ ": " ++ show (tiId ti) ++ " " ++ show (tiStatus ti)

-- | Add new named action to thread environmet.
tadd :: MVar TEnv -> String -> Act () -> IO ()
tadd tev name act = taddAt noDelay tev name act

-- | Add new action with initial delay.
taddAt :: InitialDelay -> MVar TEnv -> String -> Act () -> IO ()
taddAt f tev name act = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  if M.member name threads
    then putStrLn (name ++ " already exists.") >> return te
    else do
      now <- utcr
      let del = f now (teTimeUnit te)
      tvar <- newMVar (now+del)
      ti <- newChild act tev tvar del
      return $ te {teThreads = M.insert name ti threads
                  ,teTimes = M.insert (tiId ti) (now+del) (teTimes te) }

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
        let ti' = ti {tiStatus=Paused}
            threads' = M.update (return $ Just ti') name threads
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
newChild :: Act () -> MVar TEnv -> MVar Double -> Double -> IO ThreadInfo
newChild act tev tvar del = do
  blk <- newMVar ()
  psd <- newMVar =<< utcr
  tid <- forkIO $ do
    threadDelay (floor ((del+latency) * 1e6))
    forever $ readMVar blk >> runAct act tev (tvar,blk)
  return $ ThreadInfo tid Running blk tvar psd

-- | Pause child thread
pauseChild :: ThreadInfo -> IO ()
pauseChild ti = do
  modifyMVar_ (tiPausedTime ti) $ \_ -> utcr
  takeMVar (tiBlock ti)

-- | Resume child thread
resumeChild :: ThreadInfo -> IO ()
resumeChild ti = do
  lastPaused <- readMVar (tiPausedTime ti)
  now <- utcr
  modifyMVar_ (tiIdealTime ti) $ return . (+ (now-lastPaused))
  putMVar (tiBlock ti) ()

-- | Set time unit in thread environment.
setTimeUnit :: MVar TEnv -> Double -> IO ()
setTimeUnit tev tu = modifyMVar_ tev $ \te -> do
  return $ te {teTimeUnit = tu}

-- | Action executed in thread managed environment.
newtype Act a = Act {unAct :: ReaderT (MVar TEnv) (StateT ActState IO) a }
  deriving (Monad, MonadIO, MonadReader (MVar TEnv), MonadState ActState)

-- | State used for actions.
--
-- Pair of MVar holding last event occurence time and blocking MVar
-- for this thread.
--
type ActState = (MVar Double, MVar ())

-- | Unwrapper for Act.
runAct :: Act a -> MVar TEnv -> ActState -> IO a
runAct a e s = evalStateT (runReaderT (unAct a) e) s

-- | Delay the thread for given time unit.
rest :: Double -> Act ()
rest n = tdelay . (*n) =<< getTimeUnit

-- | Pause the thread until resumed.
-- Might be useful when action is embeddeding forever loop.
breakMe :: Act ()
breakMe = do
  (_,blk) <- get
  act $ readMVar blk

-- | Delay the thread for given seconds.
--
-- Pausing inside modifyMVar_ do block for this thread's own MVar Double.
tdelay :: Double -> Act ()
tdelay dt = when (dt > 1e-4) $ do
  (tvar,_) <- get
  act $ modifyMVar_ tvar $ \told -> do
    let tnew = told + dt
    pauseThreadUntil (tnew+latency)
    return tnew

-- | Make @Act@ from @IO@, currently it's a synonym of liftIO.
act :: IO a -> Act a
act = liftIO

-- | Get time unit from thread environment.
getTimeUnit :: Act Double
getTimeUnit = ask >>= \mv -> act (readMVar mv >>= return . teTimeUnit)

-- | Get the initialized time of TEnv, in UTCr.
getInitTime :: Act Double
getInitTime = ask >>= \mv -> act (readMVar mv >>= return . teInitTime)

-- | Get current UTCr.
getNow :: Act Double
getNow = act utcr

-- | Wait until the beginning of given multiple of TimeUnit.
--
-- When given time is non-positive, same as noDelay.
atTU :: Double -> InitialDelay
atTU n
  | n <= 0    = noDelay
  | otherwise = (\i u -> (n*u - (i `grem` (n*u))))
  where grem a b = a - (fromIntegral (fst (properFraction (a/b))) * b)

-- | No delay.
noDelay :: InitialDelay
noDelay _ _ = 0

-- | Small amount of constant value for every delays occuring in thread
-- management. Writtern as 0.1 second.
latency :: Fractional a => a
latency = 0.1

-- | Action executed in thread managed environment, old implementation.
-- newtype Act a = Act {unAct :: ReaderT (MVar TEnv) IO a}
--   deriving (Monad, MonadIO, MonadReader (MVar TEnv))

-- | Wrapper for threadDelay.
--
-- Updates reference time for each thread in teTimes Map of TEnv.
-- Using pauseThreadUntil from Sound.OpenSoundControl.
--
-- XXX: Rewrite this without looking up Map in TEnv.
--
-- tdelay :: Double -> Act ()
-- tdelay dt = when (dt > 1e-4) $ do
--   te <- ask
--   tnew' <- act $ modifyMVar te $ \te' -> do
--     mid <- myThreadId
--     let tmap = teTimes te'
--         told = maybe 0 id . M.lookup mid $ tmap
--         tnew = told + dt
--     return $ (te' {teTimes = M.update (const $ return tnew) mid tmap}, tnew)
--   act $ pauseThreadUntil (tnew'+latency)

-- | Rest for given multiple of TimeUnit.
-- Rewrote this cause it's acess to ThreadId Map is unnecessary.
--
-- To lookup MVer once, manually accessing contents of ThreadInfo Map.
-- rest :: Double -> Act ()
-- rest n = do
--   te <- ask
--   tnew' <- act $ modifyMVar te $ \te' -> do
--     mid <- myThreadId
--     let tmap = teTimes te'
--         told = maybe 0 id . M.lookup mid $ tmap
--         tnew = told + (n * teTimeUnit te')
--     return $ (te' {teTimes = M.update (const $ return tnew) mid tmap}, tnew)
--   act $ pauseThreadUntil (tnew'+latency)

test :: IO ()
test = do
  withSC3 $ \fd ->
    send fd $ d_recv $ synthdef "TM1_test"
    (out 0 $ pan2
     (sinOsc ar (control kr "freq" 440) 0 * 0.3 *
      xLine kr 1 1e-5 (control kr "dur" 200e-3) RemoveSynth)
     (control kr "pan" 0) 1)

  let pong f p t = withSC3 $ flip send $
        Bundle (UTCr $ t+0.1)
          [s_new "TM1_test" (-1) AddToTail 1 [("freq",f),("pan",p)]]

  e <- initEnv

  taddAt (atTU 4) e "thread_01" $ do
    now <- getNow
    act $ pong 440 0 now
    rest 0.5

  randomRIO (10^6,5*10^6) >>= threadDelay

  taddAt (atTU 4) e "thread_02" $ do
    now <- getNow
    act $ pong 330 (-1) now
    rest 1

  randomRIO (10^6,5*10^6) >>= threadDelay

  taddAt (atTU 4) e "thread_03" $ do
    now <- getNow
    act $ pong 550 1 now
    rest 2

  threadDelay (8*10^6)

  tkill e "thread_01"
  tkill e "thread_02"
  tkill e "thread_03"

main :: IO ()
main = test