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
-- One of the things not have been done is choosing elements from list
-- with specifying probability. Take a look at hackage in random
-- category and check whether there exist a function that give similar
-- result from Pwrand. Looking for a weighted random element generator.
--

module SCHelp.PG.Cookbook02 (
  main,
  -- * Merging (interleaving) independent streams
  -- $mergingIndependent
  fitInRange,
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
  thePitches
  -- * Changing Pbind value patterns on the fly
  -- $changingPbind

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
     send,
     wait)
import Sound.SC3
    (AddAction(..),
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

-- | Sends merged melody to scsynth and mekes sound. Synth used to
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

-- | Fit in specified range with using @mod@.
fitInRange :: Integral a => a -> a -> a -> a
fitInRange min max target
    | target < min = fromIntegral (target `mod` min)
    | max < target = fromIntegral (target `mod` max)
    | otherwise = target

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
main = runMovingIndex

-- $readingArray
--
-- Read an element from array, and move back and forth with in the
-- array.
--
-- One of the problem for haskell might be where to hold the temporary
-- variable for amount of step in the array. Using control bus for
-- holding step value.
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
-- TBW
--