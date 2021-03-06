------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook02
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook02_Manipulating_Patterns/.
--
-- One of the thing not have been done is choosing elements from list
-- with specifying probability. Take a look at hackage in random
-- category and check whether there exist a function that give similar
-- result from Pwrand. Looking for a weighted random element generator.
--

module SCHelp.PG.Cookbook02 (
  main,
  -- * Merging (interleaving) independent streams
  -- $mergingIndependent
  mkMelodyLine,
  lowMelody,
  highMelody,
  melodyWriter,
  melodyReader,
  mkMelody,
  melodyToNotes,
  spawnMerging,


  -- * Reading an array forward and backward arbitrarily
  -- $readingArray
  runMovingIndex,
  movingBusNum,
  writeIndex,
  writeIndex',
  getMove,
  setMove,
  thePitches,

  -- * Changing Pbind value patterns on the fly
  -- $changingPbind
  mkNote,
  runChangingValues,
  runChanging,
  changingDegreeBuf,
  changingDurBuf,
  getDegrees,
  setDegrees,
  getDurs,
  setDurs
  ) where

import Control.Applicative
    ((<$>),
     (<*>))
import Control.Concurrent
    (forkIO,
     threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
    (Chan,
     newChan,
     getChanContents,
     writeChan)
import Control.Monad.State
import System.Random
    (RandomGen,
     newStdGen,
     getStdRandom,
     randomR)
import FRP.Reactive
    (TimeT,
     Event,
     listE)
import Sound.OpenSoundControl
    (OSC(..),
     Datum(..),
     Time(..),
     pauseThreadUntil,
     send,
     utcr,
     wait)
import Sound.SC3
    (AddAction(..),
     b_alloc,
     b_getn,
     b_setn,
     c_get,
     c_set,
     s_new,
     withSC3)
import Sound.SC3.Lang.Math

import Reusable
import SCQuery
import SCTree
import SCSched


-- $mergingIndependent
--
-- This example uses two melodies: lowMelody and highMelody.
-- Those are generated in series, and ugen sends new synth with choosing
-- frequency value from one, each time.
--
-- Tried to use concurrency, but made the program unnecessarily cpu
-- and memory expensive. Rewrote to non-concurrent version.

mkMelodyLine :: Double -> [Double] -> IO [Double]
mkMelodyLine start intervals =
    scanl (+) start <$> choices intervals <$> newStdGen

lowMelody :: IO [Double]
lowMelody = map (fromIntegral . fitInRange (-7) 11 . round) <$>
            mkMelodyLine 4 [-2,-1,1,2]

highMelody :: IO [Double]
highMelody = map (fromIntegral . fitInRange 7 18 . round) <$>
             mkMelodyLine 14 [-3,-2,2,3]

mkMelody :: (RandomGen g) => [Double] -> [Double] -> g -> [Double]
mkMelody [] _ _ = []
mkMelody _ [] _ = []
mkMelody lows highs g =
    if isHigh
      then head highs : mkMelody lows (tail highs) g'
      else head lows : mkMelody (tail lows) highs g'
    where
      (isHigh, g') = randomR (False,True) g

melodyToNotes :: [Double] -> IO (Event OSC)
melodyToNotes vals = do
  durs <- choices [0.25, 0.5] <$> newStdGen
  let vals' = map (f' . f) vals
      f d = freq $ defaultPitch { degree = d }
      f' freq = s_new "simpleSynth" (-1) AddToTail 1 [("freq", freq)]
  return $ listE $ zip (scanl (+) 0 durs) vals'

-- | Sends merged melody to scsynth and makes sound. Synth used to
-- play the melody is @simpleSynth@ from Cookbook01.
spawnMerging :: IO ()
spawnMerging = do
  low <- lowMelody
  high <- highMelody
  notes <- mkMelody low high <$> newStdGen
  notes' <- melodyToNotes notes
  spawn 1 60 notes'

-- | Concurrent melody writer.
melodyWriter :: Chan Double -> [Double] -> [Double] -> IO ()
melodyWriter chan low high = do
  isHigh <- getStdRandom (randomR (False,True))
  if isHigh
     then writeChan chan (head high) >> melodyWriter chan low (tail high)
     else writeChan chan (head low) >> melodyWriter chan (tail low) high

-- | Concurrent melody reader.
melodyReader :: Chan Double -> IO (Event OSC)
melodyReader chan = do
  vals <- getChanContents chan
  durs <- choices [0.25,0.5] <$> newStdGen
  let vals' = map (f2 . f1) vals
      f1 d = freq $ defaultPitch {degree=d}
      f2 freq = s_new "simpleSynth" (-1) AddToTail 1 [("freq",freq)]
  return $ listE $ zip (scanl (+) 0 durs) vals'


main :: IO ()
main = runChangingValues

-- $readingArray
--
-- Read an element from array, and move back and forth with in the
-- array.
--
-- One of the problem for haskell might be where to hold the temporary
-- variable for amount of step in the array. Using control bus for
-- holding step value.
--
-- Used MVar to hold the current index. The main action
-- @runMovingIndex@ forks the writer thread, and loops the main reader
-- action forever.
--
-- One obvious difference between original sclang version is that
-- haskell version changes the moving step value immediately after the
-- buffer's value has changed. Sclang version waits to update the step
-- value until the last looping of pitch array has ended.
--

-- | Runs moving index example. Set the value of buffer to hold the
-- value for moving step.
--
-- > > setMove 3
-- > > forkIO (runMovingIndex)
-- > > setMove 4
-- > > setMove 7
--
runMovingIndex :: IO ()
runMovingIndex = do
  var <- newMVar 0
  forkIO (writeIndex var)
  forever $ do
    idx <- takeMVar var
    let theFreq = freq $ defaultPitch { degree = degree }
        degree = thePitches !! idx
    withSC3 $ \fd -> send fd $
                     s_new "simpleSynth" (-1) AddToTail 1 [("freq",theFreq)]
    threadDelay $ round $ 10 ^ 6 * 0.25

movingBusNum :: Num a => a
movingBusNum = 101

thePitches :: [Double]
thePitches = [0..14]

writeIndex :: MVar Int -> IO ()
writeIndex var = forever $ do
  evalStateT (sequence_ (repeat (writeIndex' var))) 0

writeIndex' :: MVar Int -> StateT Int IO ()
writeIndex' var = do
  idx <- get
  mv <- liftIO getMove
  let idx' = (idx + round mv) `mod` (length thePitches - 1)
  liftIO $ putMVar var idx'
  put idx'

getMove :: IO Double
getMove = withSC3 work >>= return . parse
    where
      parse (Message "/c_set" [_,Float v]) = v
      parse _ = error "Something wrong happened in \"/c_get\""
      work fd = send fd (c_get [movingBusNum]) >> wait fd "/c_set"

setMove :: Double -> IO ()
setMove move =
  withSC3 $ \fd -> send fd (c_set [(movingBusNum, move)])

-- $changingPbind
--
-- Using buffer to hold the values used in synth.  In this way, random
-- value pattern could not be expressed.
-- 
-- Delay between getting new values from scsynth for degree and
-- durations are hearedable.
--
-- Try something like:
--
-- > > setDegrees (take 32 $ cycle [1,3,5,7])
-- > > setDurs (take 32 $ repeat 0.5)
-- > > t1 <- forkIO (runChangingValues)
-- > > setDurs (take 32 $ cycle [0.25, 0.25, 0.5])
-- > > setDegrees . take 32 =<< choices [0,1,3,5,7] <$> newStdGen
-- > > killThread t1
--
runChangingValues :: IO ()
runChangingValues = join (runChanging <$> utcr <*> getDurs <*> getDegrees)

-- | Repeat sending message with getting values from buffers.
-- 
-- Timing would be adjusted when either of degrees or durations has
-- reached to the last value.
-- 
runChanging :: Double -> [Double] -> [Double] -> IO ()
runChanging t0 (x:xs) (y:ys) = do
  let latency = 0.1
  withSC3 $ \fd -> send fd (Bundle (UTCr $ t0 + latency) [mkNote y])
  pauseThreadUntil (x + t0)
  runChanging (x + t0) xs ys
runChanging t0 _ _ = join (runChanging t0 <$> getDurs <*> getDegrees)
  
mkNote :: Double -> OSC
mkNote deg = msg
  where msg = s_new "simpleSynth" (-1) AddToTail 1 [("freq",f)]
        f = freq $ defaultPitch {degree=deg}

changingDegreeBuf :: Num a => a
changingDegreeBuf = 101

changingDurBuf :: Num a => a
changingDurBuf = 102

getDegrees :: IO [Double]
getDegrees = getBuf changingDegreeBuf

getDurs :: IO [Double]
getDurs = getBuf changingDurBuf

setDegrees :: [Double] -> IO ()
setDegrees = setBuf changingDegreeBuf

setDurs :: [Double] -> IO ()
setDurs = setBuf changingDurBuf

setBuf :: Int -> [Double] -> IO ()
setBuf bufNum vals =
    withSC3 $ \fd -> do
        send fd (b_alloc bufNum (length vals) 1)
        wait fd "/done"
        send fd (b_setn bufNum [(0, vals)])

getBuf :: Int -> IO [Double]
getBuf bufNum = do
  bufInfo <- getBufInfo bufNum
  osc <- withSC3 $ \fd -> do
    send fd (b_getn bufNum [(0, bufNumFrames bufInfo)])
    wait fd "/b_setn"
  return $ toDoubles osc
    where
      toDoubles (Message "/b_setn" (_:_:_:fs)) = map unFloat fs
      toDoubles _ = []
      unFloat (Float x) = x
      unFloat _ = error "Not a float"
