{-# LANGUAGE TypeSynonymInstances #-}
------------------------------------------------------------------------------
-- | Playing with threads in haskell.
--
module ThreadPlay where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio
import System.Random
import System.IO.Unsafe

import Sound.SC3
import Sound.OpenSoundControl

import SCQuery hiding (latency)
import SCTree
import Reusable

master01 :: IO ()
master01 = do
  mvar <- newMVar ()
  gen <- newStdGen
  let childIds = take 10 $ randomRs (0,maxBound) gen
      childDelays = randomRs (10,1000) gen
  sequence_ $ getZipList $ goChild mvar <$> ZipList childIds <*> ZipList childDelays

goChild :: MVar () -> Int -> Int -> IO ThreadId
goChild mv cId del = forkIO $ do
  takeMVar mv
  putStrLn $ "ID: " ++ show cId ++ " del: " ++ show del
  threadDelay (del * 10 ^ 3)
  putStrLn $ "done ID: " ++ show cId
  putMVar mv ()

type ThreadBank = (Int,Map Int (ThreadId,MVar ()))

newThreadBank :: IO (MVar ThreadBank)
newThreadBank = newMVar (0,M.empty)

addThread :: MVar ThreadBank -> ChildThread () -> IO ThreadBank
addThread mvar act = do
  (currentId,bank) <- takeMVar mvar
  childVar <- newMVar ()
  tid <- forkIO $ runReaderT act childVar
  let currentId' = currentId + 1
      bank' = M.insert currentId' (tid,childVar) bank
  putMVar mvar (currentId',bank')
  return (currentId',bank')

deleteThread  :: MVar ThreadBank -> Int -> IO ThreadBank
deleteThread mvar key = do
  b@(currentId,bank) <- takeMVar mvar
  case M.lookup key bank of
    Just (tid,_) -> do
      killThread tid
      let retval = (currentId, M.delete key bank)
      putMVar mvar retval >> return retval
    Nothing -> do
      putMVar mvar b >> return b

lookupThreadId :: MVar ThreadBank -> Int -> IO (Maybe ThreadId)
lookupThreadId mvar key = do
  (_,bank) <- readMVar mvar
  return . (fmap fst) $ M.lookup key bank

showThreadBank :: MVar ThreadBank -> IO ()
showThreadBank mvar = do
  (_,bank) <-  readMVar mvar
  mapM_ print $ M.toList $ M.map fst bank

type ChildThread a = ReaderT (MVar ()) IO a

thDelay :: Int -> ChildThread ()
thDelay time = do
  mv <- ask
  lift $ do {takeMVar mv; threadDelay time; putMVar mv ()}

data Schedule = Act (IO ()) | Pause Int

sched01 :: [Schedule]
sched01 =
    [Act $ putStrLn "hello",
     Pause $ 10 ^ 6,
     Act $ putStrLn "world"]

sched02 :: [Schedule]
sched02 =
    [
     Act $ f ["dur":=1.5],
     Pause $ 5 * 10 ^ 5,
     Act $ f ["freq":=660,"dur":=2.5],
     Pause $ 5 * 10 ^ 5,
     Act $ f ["freq":=330,"dur":=0.25],
     Pause $ 5 * 10 ^ 5,
     Act $ f ["freq":=660],
     Pause $ 5 * 10 ^ 5
    ]
    where
      f ps = query (add 1 $ Synth (-1) "percussive01" ps) s

sched03 :: IO [Schedule]
sched03 = do
  g <- newStdGen
  let (f1:f2:f3:_) = randomRs (-1,1::Double) g
      (p1:p2:p3:_) = randomRs (-1,1::Double) g
  return [
    Act $ f ["dur":=2,"freq":=220+f1,"pan":=p1],
    Pause $ 10 * 10 ^ 5,
    Act $ f ["dur":=2,"freq":=330+f2,"pan":=p2],
    Pause $ 10 * 10 ^ 5,
    Act $ f ["dur":=4,"freq":=160+f3,"pan":=p3],
    Pause $ 20 * 10 ^ 5
   ]
  where
    f ps = query (add 1 $ Synth (-1) "percussive01" ps) s

sched04 :: [Schedule]
sched04 =
    [
     Act $ f ["dur":=0.5,"freq":=880,"pan":=0.25],
     Pause $ 2 * 10 ^ 5,
     Act $ f ["dur":=0.5,"freq":=1320,"pan":=0.25],
     Pause $ 2 * 10 ^ 5
    ]
  where
    f ps = query (add 1 $ Synth (-1) "percussive01" ps) s

sched05 :: [Schedule]
sched05 =
    [
     Act $ f ["dur":=0.5,"freq":=660,"pan":=(-0.25)],
     Pause $ 2 * 10 ^ 5,
     Act $ f ["dur":=0.5,"freq":=990,"pan":=(-0.25)],
     Pause $ 2 * 10 ^ 5
    ]
  where
    f ps = query (add 1 $ Synth (-1) "percussive01" ps) s

piece01 :: MVar ThreadBank -> IO ()
piece01 mv = do
  sched03' <- sched03
  addSchedules mv (cycle sched02)
  addSchedules mv (cycle sched03')
  addSchedules mv (cycle sched04)
  addSchedules mv (cycle sched05)

addSchedules :: MVar ThreadBank -> [Schedule] -> IO ()
addSchedules var sched = do
  (curId, bank) <- takeMVar var
  childVar <- newMVar ()
  tid <- forkIO $ mapM_ (goSchedule childVar) sched
  let bank' = M.insert (curId+1) (tid,childVar) bank
  putMVar var (curId+1,bank')
    where
      goSchedule :: MVar () -> Schedule -> IO ()
      goSchedule var sched =
          do let go (Act act) = act
                 go (Pause i) = takeMVar var >> threadDelay i >> putMVar var ()
             go sched

thread01 :: ChildThread ()
thread01 = do
  lift $ putStrLn "hello"
  thDelay (2 * 10 ^ 6)
  lift $ putStrLn "world"

percussive01 :: UGen
percussive01 = out 0 $ pan2 (sinOsc ar ("freq" @= 440) 0 * env) ("pan"@=0) 1
    where env = envGen kr 1 ("amp"@=0.1) 0 1 RemoveSynth
                (envPerc 0.01 ("dur"@=1))


at :: Double -> (Double -> IO Double) -> IO t
at t f = do
  n <- f t
  pauseThreadUntil (n + t)
  at (n + t) f

ats :: Double -> [Double -> IO Double] -> IO ()
ats t (f:fs) = do
  n <- f t
  pauseThreadUntil (n+t)
  ats (n+t) fs
ats _ [] = return ()

ats' :: Double -> [IO Double] -> IO ()
ats' t (f:fs) = do
  n <- f
  pauseThreadUntil (n+t)
  ats' (n+t) fs
ats' _ [] = return ()

latency :: Fractional a => a
latency = 0.01

es1 :: [Double -> IO Double]
es1 = [\t -> withSC3 $ \fd -> do {go t fd [("freq",440)]; return 0.5},
       \t -> withSC3 $ \fd -> do {go t fd [("freq",330)]; return 0.5},
       \t -> withSC3 $ \fd -> do {go t fd [("freq",220)]; return 0.25},
       \t -> withSC3 $ \fd -> do {go t fd [("freq",330)]; return 0.25},
       \t -> withSC3 $ \fd -> do {go t fd [("freq",440)]; return 0.25},
       \t -> withSC3 $ \fd -> do {go t fd [("freq",330)]; return 0.25}]
    where go t fd ps = send fd $ Bundle (UTCr $ t + latency) $
                       [s_new "percussive01" (-1) AddToTail 1 (("pan",0):ps)]

es2  :: [Double -> IO Double]
es2 = [go 440 (1%4), go 330 (1%8), go 220 (1%8),
       go 220 (1%8), go 110 (1%8), go 220 (1%8), go 110 (1%8),
       go 330 (1%8), go 110 (1%8), go 220 (1%4),
       go 330 (1%6), go 110 (1%6), go 220 (1%6)]
    where
      go freq rest = \t -> withSC3 $ \fd -> do
                       foffset <- randomRIO (-0.1,0.1)
                       aoffset <- randomRIO (0,0.2)
                       send fd $ Bundle (UTCr $ t + latency) $
                         [s_new "percussive01" (-1) AddToTail 1
                          [("freq",freq+foffset),
                           ("pan",1),
                           ("dur",0.1),
                           ("amp",0.1+aoffset)]]
                       return (fromRational rest)


-- | Do these:
--
-- > > t1 <- forkIO $ do {now <- utcr; ats (fromIntegral $ ceiling now) (cycle es3)}
-- > > t2 <- forkIO $ do {now <- utcr; ats (fromIntegral $ ceiling now) (cycle es4)}
--
es3 :: [Double -> IO Double]
es3 = [go 60 (1%4), go 67 (1%4), go 67 (1%4), go 60 (1%4),
       go 65 (1%4), go 65 (1%4), go 60 (1%4), go 62 (1%4)]
    where
      go pitch rest = \t -> do
        withSC3 $ \fd -> send fd $ bundle (t + latency) [
                             s_new "percussive01" (-1) AddToTail 1
                                   [("freq",midiCPS pitch),("dur",0.5)]]
        return $ fromRational rest

es4 :: [Double -> IO Double]
es4 = [go 48 63 72 79 (4%1),
       go 48 57 65 79 (4%1),
       go 48 63 72 79 (4%1),
       go 48 57 65 79 (4%1),
       go 48 63 72 79 (4%1),
       go 50 57 69 79 (4%1),
       go 49 62 67 74 (4%1),
       go 49 65 71 77 (4%1)]
    where
      go' p1 p2 p3 p4 t = mapM_ (\f -> withSC3 $ \fd -> send fd $ bundle (t + latency)
                                     [s_new "percussive01" (-1) AddToTail 1
                                            [("freq",midiCPS f),("dur",4),("amp",0.1)]])
                         [p1,p2,p3,p4]
      go p1 p2 p3 p4 rest = \t -> do
          go' p1 p2 p3 p4 t
          return $ fromRational rest


-- | A function that takes 3 args.
zipme :: Int -> Int -> Int -> Int
zipme a b c = a + b + c

-- | Zipping with @zipme@.
zipWithZipme :: [Int]
zipWithZipme = getZipList $ zipme <$>
               ZipList [1..] <*> ZipList [11..] <*> ZipList [101..111]

-- | An io action that takes 3 args.
zipmeM :: Int -> Int -> Int -> IO Int
zipmeM a b c = return $ a + b + c

-- | Zipping with @zipmeM@.
zipWithZipmeM :: IO [Int]
zipWithZipmeM = sequence $ getZipList $
                zipmeM <$> z [1..] <*> z [11..] <*> z [101..111]
                    where z = ZipList

goEcho :: IO ThreadId
goEcho = do
  var <- newEmptyMVar
  forkIO $ echo var

echo :: MVar () -> IO ()
echo c = do
  msg <- takeMVar c
  print msg

type IntFunc = Int -> Int -> IO Int

goFunc = do
  var <- newMVar (\a b -> return (a + b))
  forkIO $ func var

func :: MVar IntFunc -> IO ()
func c = do
  f <- takeMVar c
  result <- f 1 2
  print result

goPrimeAndFib :: IO ()
goPrimeAndFib = do
  ps <- run primes
  fs <- run fibonacci
  mapM_ print $ zip ps fs
 where
   run f = do
     c <- newChan
     l <- getChanContents c
     forkIO $ writeList2Chan c f
     return l

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p > 0]

fibonacci :: [Integer]
fibonacci = 0:1:zipWith (+) fibonacci (tail fibonacci)
