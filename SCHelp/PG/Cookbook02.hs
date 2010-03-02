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

  mkMelodyLine,
  melodyWriter,
  melodyReader,
  main

  -- * Reading an array forward and backward arbitrarily
  -- $readingArray

  -- * Changing Pbind value patterns on the fly
  -- $changingPbind

  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
    (Chan,
     newChan,
     getChanContents,
     writeChan)
import System.Random
    (newStdGen,
     getStdRandom,
     randomR)
import FRP.Reactive
    (TimeT,
     listE)
-- import Sound.OpenSoundControl
-- import Sound.SC3
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

-- data Melodies = Melodies {
--       highMelody :: [Double],
--       lowMelody :: [Double]
--     } deriving (Eq, Show)


mkMelodyLine :: Double -> [Double] -> IO [Double]
mkMelodyLine start intervals =
    scanl (+) start <$> choices intervals <$> newStdGen

data MelodyHight = HighMelody Double
                 | LowMelody Double
                   deriving (Eq, Show)

lowMelody :: IO [Double]
lowMelody = mkMelodyLine 4 [-2,-1,1,2]

highMelody :: IO [Double]
highMelody = mkMelodyLine 14 [-3,-2,2,3]

melodyWriter :: Chan Double -> [Double] -> [Double] -> IO ()
melodyWriter chan low high = do
  isHigh <- getStdRandom (randomR (False,True))
  if isHigh
     then writeChan chan (head high) >> melodyWriter chan low (tail high)
     else writeChan chan (head low) >> melodyWriter chan (tail low) high

melodyReader :: Chan Double -> IO [(TimeT,Double)]
melodyReader chan = do
  vals <- getChanContents chan
  durs <- choices [0.25,0.5] <$> newStdGen
  return $ zip (scanl (+) 0 durs) vals

main :: IO ()
main = do
  chan <- newChan
  low <- lowMelody
  high <- highMelody
  _ <- forkIO $ melodyWriter chan low high
  pairs <- melodyReader chan
  print pairs


-- $readingArray
--
-- TBW


-- $changingPbind
--
-- TBW
