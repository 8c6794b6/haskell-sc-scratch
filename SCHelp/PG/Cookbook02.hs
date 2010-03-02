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
-- /PG_Cookbook02_Manipulating_Patterns/
--

module SCHelp.PG.Cookbook02 (

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
  main

  -- * Reading an array forward and backward arbitrarily
  -- $readingArray

  -- * Changing Pbind value patterns on the fly
  -- $changingPbind

  ) where

import Control.Applicative 
    ((<$>),
     (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
    (Chan,
     newChan,
     getChanContents,
     writeChan)
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
    (OSC(..))
import Sound.SC3
    (AddAction(..),
     s_new)
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

fitInRange :: Integral a => a -> a -> a -> a
fitInRange min max target 
    | target < min = fromIntegral (target' `mod` min)
    | max < target = fromIntegral (target' `mod` max)
    | otherwise = target
    where target' = target

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
main = spawnMerging

-- $readingArray
--
-- TBW


-- $changingPbind
--
-- TBW
