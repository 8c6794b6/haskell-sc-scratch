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
  Melodies(..),
  melodies,
  mkMelodyLine,
  runMerging
  -- * Reading an array forward and backward arbitrarily
  -- $readingArray

  -- * Changing Pbind value patterns on the fly
  -- $changingPbind
  
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.Random

import FRP.Reactive
import Sound.OpenSoundControl
import Sound.SC3

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

data Melodies = Melodies { 
      highMelody :: [Double],
      lowMelody :: [Double] 
    } deriving (Eq, Show)

type Merging a = StateT Melodies IO a

melodies :: IO Melodies
melodies = do
  high <- mkMelodyLine 4 [-2, -1, 1, 2]
  low <- mkMelodyLine 14 [-3, -2, 2, 3]
  return $ Melodies high low

mkMelodyLine :: Double -> [Double] -> IO [Double]
mkMelodyLine start intervals =
    scanl (+) start <$> choices intervals <$> newStdGen

runMerging :: Merging (Event OSC)
runMerging = do
  g <- liftIO $ newStdGen
  let durs = choices [0.25,0.5] g
  return undefined
  -- return ()

-- $readingArray
-- 
-- XXX: TBW

-- $changingPbind
-- XXX: TBW

