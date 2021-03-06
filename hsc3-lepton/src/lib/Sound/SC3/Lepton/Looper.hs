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
-- Simple manager for looping forked threads.  Main target usecase is for interactive
-- management of audible events, though might be useful for other purposes.
--
module Sound.SC3.Lepton.Looper where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as M

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Sound.OSC


{-$example

Initialize:

> > e <- initEnv

Write actions:

> > let action1 = do {act $ print "FOO!"; rest 2}

Add action:

> > tadd e (tu 4) "one" action1

Pause and resume the thread:

> > tpause e (tu 4) "one"
> > tresume e (tu 4) "one"

Change TimeUnit:

> > setTimeUnit e 0.5
> > tresume0 e "one"

Kill thread:

> > tkill e "one"

Run multiple threads:

> > let action2 = do { ... }
> > let action3 = do { ... }
> > tadd (tu 1) e "one" action1
> > tadd (tu 1) e "two" action2
> > tadd (tu 2) e "three" action3

-}

-- | Environment for managing threads.
data TEnv = TEnv
  { -- | UTCr from OSC.
    teInitTime :: Double

    -- | Duration of one unit.
  , teTimeUnit :: Double

    -- | Map of ThreadInfo.
  , teThreads :: M.Map String ThreadInfo

    -- | Map of ThreadId and last execution time. Currently unused.
  , teTimes :: M.Map ThreadId Double
  }

instance Show TEnv where
  show (TEnv _ b t _) =
    "TEnv - TimeUnit: " ++ show b ++ ", threads: " ++ show (M.size t)

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

-- | Initialize environment for threads with TimeUnit = 1 second.
initEnv :: IO (MVar TEnv)
initEnv = initEnvTU 1

-- | Initialize environment with given TimeUnit.
initEnvTU :: Double -> IO (MVar TEnv)
initEnvTU u = time >>= \t0 -> newMVar (TEnv t0 u M.empty M.empty)

-- | Dump the contents of thread environment.
dumpEnv :: MVar TEnv -> IO ()
dumpEnv tev = do
  te <- readMVar tev
  putStrLn $ '\n' : show te
  forM_ (M.assocs $ teThreads te) $ \(n,ti) ->
    putStrLn $ n ++ ": " ++ show (tiId ti) ++ " " ++ show (tiStatus ti)

-- | Add new action with initial delay.
tadd :: InitialDelay -> MVar TEnv -> String -> Act () -> IO ()
tadd f tev name a = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  if M.member name threads
    then putStrLn (name ++ " already exists.") >> return te
    else do
      now <- time
      let del = f now (teTimeUnit te)
      tvar <- newMVar (now+del)
      ti <- newChild a tev name tvar del
      return $ te {teThreads = M.insert name ti threads
                  ,teTimes = M.insert (tiId ti) (now+del) (teTimes te) }

-- | Update action.
--
-- When given name does not exist, silently add given action.
--
tupdate :: InitialDelay -> MVar TEnv -> String -> Act () -> IO ()
tupdate f tev name a = do
  te <- readMVar tev
  let threads = teThreads te
  _ <- if not $ M.member name threads
     then forkIO (tadd f tev name a)
     else forkIO $ do
       t0 <- time
       threadDelay $ floor $ (latency + f t0 (teTimeUnit te)) * 1e6
       tkill0 tev name
       tadd0 tev name a
  return ()

-- | Pause action with specifying delay.
tpause :: InitialDelay -> MVar TEnv -> String -> IO ()
tpause f tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> case tiStatus ti of
      Paused  -> return te
      Running -> do
        _ <- forkIO $ do
          t0 <- time
          threadDelay $ floor $ (latency + f t0 (teTimeUnit te)) * 1e6
          pauseChild ti
        let ti' = ti {tiStatus=Paused}
            threads' = M.update (return $ Just ti') name threads
        return $ te {teThreads = threads'}

-- | Resume action after specified delay time.
tresume :: InitialDelay -> MVar TEnv -> String -> IO ()
tresume f tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> case tiStatus ti of
      Running -> return te
      Paused  -> do
        _ <- forkIO $ do
          t0 <- time
          threadDelay $ floor $ (latency + f t0 (teTimeUnit te)) * 1e6
          resumeChild ti
        let ti' = ti {tiStatus=Running}
            threads' = M.update (return $ Just ti') name threads
        return $ te {teThreads = threads'}

-- | Kill action after specified delay time.
tkill :: InitialDelay -> MVar TEnv -> String -> IO ()
tkill f tev name = modifyMVar_ tev $ \te -> do
  let threads = teThreads te
  case M.lookup name threads of
    Nothing -> return te
    Just ti -> do
      t0 <- time
      _ <- forkIO $ do
        threadDelay $ floor $ (latency + f t0 (teTimeUnit te)) * 1e6
        killThread (tiId ti)
      return $ te {teThreads = M.delete name threads}

-- | Add new named action to thread environmet, immediately.
tadd0 :: MVar TEnv -> String -> Act () -> IO ()
tadd0 = tadd noDelay

-- | Kill action.
tkill0 :: MVar TEnv -> String -> IO ()
tkill0 = tkill noDelay

-- | Pause action.
tpause0 :: MVar TEnv -> String -> IO ()
tpause0 = tpause noDelay

-- | Resume action.
tresume0 :: MVar TEnv -> String -> IO ()
tresume0 = tresume noDelay

------------------------------------------------------------------------------
--
-- For child threads
--
------------------------------------------------------------------------------

-- | Fork child thread.
newChild :: Act () -> MVar TEnv -> String -> MVar Double -> Double -> IO ThreadInfo
newChild a tev name tvar del = do
  blk <- newMVar ()
  psd <- newMVar =<< time
  let as = ActState name blk tvar
  tid <- forkIO $ do
    threadDelay (floor ((del+latency) * 1e6))
    forever $ readMVar blk >> runAct a tev as
  return $ ThreadInfo tid Running blk tvar psd

-- | Pause child thread
pauseChild :: ThreadInfo -> IO ()
pauseChild ti = do
  modifyMVar_ (tiPausedTime ti) $ \_ -> time
  takeMVar (tiBlock ti)

-- | Resume child thread
resumeChild :: ThreadInfo -> IO ()
resumeChild ti = do
  lastPaused <- readMVar (tiPausedTime ti)
  now <- time
  modifyMVar_ (tiIdealTime ti) $ return . (+ (now-lastPaused))
  putMVar (tiBlock ti) ()

------------------------------------------------------------------------------
--
-- Updating thread environment
--
------------------------------------------------------------------------------

-- | Set time unit in thread environment.
setTimeUnit :: MVar TEnv -> Double -> IO ()
setTimeUnit tev u = modifyMVar_ tev $ \te -> return $ te {teTimeUnit = u}

------------------------------------------------------------------------------
--
-- Action
--
------------------------------------------------------------------------------

-- | Action executed in thread managed environment.
newtype Act a = Act {unAct :: ReaderT (MVar TEnv) (StateT ActState IO) a }
  deriving (Monad, MonadIO, MonadReader (MVar TEnv), MonadState ActState)

-- | State used for actions.
data ActState = ActState
  { -- | Name of this thread.
    asName :: String
    -- | MVar to block this thread.
  , asBlock :: MVar ()
    -- | MVar to hold last executed time for delaying.
  , asIdealTime :: MVar Double }

-- | Unwrapper for Act.
runAct :: Act a -> MVar TEnv -> ActState -> IO a
runAct a e = evalStateT (runReaderT (unAct a) e)

-- | Make @Act@ from @IO@, currently it's a synonym of liftIO.
act :: IO a -> Act a
act = liftIO

-- | Delay the thread for given time unit.
rest :: Double -> Act ()
rest n = tdelay . (*n) =<< getTimeUnit

-- | Enable manager to pause the thred at specific point.
-- Will pause itself when manager paused this thread.
-- Might be useful when action has own forever loop.
pauseHere :: Act ()
pauseHere = do
  ActState _ blk _ <- get
  act $ readMVar blk

-- | Kill this thread and delete from thread environment.
done :: Act ()
done = do
  tev <- ask
  ActState name _ _ <- get
  act $ do
    modifyMVar_ tev $ \te ->
      return $ te {teThreads = M.delete name $ teThreads te}
    killThread =<< myThreadId

-- | Delay the thread for given seconds.
--
-- Pausing inside modifyMVar_ do block for this thread's own MVar Double.
tdelay :: Double -> Act ()
tdelay dt = when (dt > 1e-4) $ do
  ActState _ _ tvar <- get
  act $ modifyMVar_ tvar $ \told -> do
    let tnew = told + dt
    pauseThreadUntil (tnew+latency)
    return tnew

-- | Get time unit from thread environment.
getTimeUnit :: Act Double
getTimeUnit = ask >>= \mv -> act (teTimeUnit `fmap` readMVar mv)

-- | Get the initialized time of TEnv, in UTCr.
getInitTime :: Act Double
getInitTime = ask >>= \mv -> act (teInitTime `fmap` readMVar mv)

-- | Get current UTCr.
getNow :: Act Double
getNow = act time

-- | Run action with empty env and state.
testAct :: Act a -> IO a
testAct a = do
  e <- newEmptyMVar
  b <- newEmptyMVar
  d <- newEmptyMVar
  runAct a e (ActState "test" b d)

-- | Wait until the beginning of given multiple of TimeUnit.
--
-- Acts immediately when given time is non-positive.
tu :: Double -> InitialDelay
tu n | n <= 0    = noDelay
     | otherwise = \i u -> (n*u - (i `grem` (n*u)))
  where
    grem a b = a - (fromIntegral (fst (properFraction (a/b)) :: Integer) * b)

-- | No delay.
noDelay :: InitialDelay
noDelay _ _ = 0

-- | Small amount of constant value for every delays occuring in thread
-- management. In short, 0.1 second.
latency :: Fractional a => a
latency = 0.1
