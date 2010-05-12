------------------------------------------------------------------------------
-- |
-- Exercise from /Stream-Patterns-Events1/.
--

module SCHelp.SPE.Part1 where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.Map (Map)
import System.Random
import qualified Data.Map as M

import Control.Applicative.State
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Monadic
import Sound.SC3.Wing
import qualified Sound.SC3.Wing.UGen.ControlArg as A

main :: IO ()
main = runPartOne

-- | Sends synthdef used in this example.
setPartOne :: Transport t => t -> IO OSC
setPartOne fd = do
  ug <- help_SPE1 
  loadSynthdef "help_SPE1" ug fd

-- | Runs the sound example shown in SPE sc help file Part 1.
runPartOne :: IO ()
runPartOne = spawn 0 120 . listE . 
             zip durs . evalState oscs =<< newStdGen
    where
      durs = scanl (+) 0 $ repeat 0.25
      oscs = concat <$> (sequence $ repeat r00)

-- | The only synthdef used in this example
help_SPE1 :: IO UGen
help_SPE1 = do
  nz <- ((*36) >>> (+110) >>> midiCPS) <$> lfNoise1 kr 1
  dl <- randomRs (0,0.05) <$> newStdGen
  dr <- randomRs (0,0.05) <$> newStdGen
  let evl = envGen kr 1 1 0 1 RemoveSynth shp * 0.3
      shp = envPerc 0.01 1
      v = rlpf (lfSaw ar A.freq 0 * evl) nz 0.1
      f a b = allpassN b 0.05 a 4
      sig = foldr f v (take 4 $ zipWith mce2 dl dr)
  return $ out A.i_out sig

-- | Makes osc message from midi note pitch.
new_help_SPE1 :: Double -> OSC
new_help_SPE1 note = s_new "help_SPE1" (-1) AddToTail 1 
                     [("freq", midiCPS note)]

-- | Updates StdGen seed.
updateGen :: State StdGen StdGen
updateGen = get >>= \g -> (put . snd . next $ g) >> return g

-- | Repeat the state transformation for random number of time in
-- given range.
repeatFor :: (Int,Int) -> State StdGen [OSC] -> State StdGen [OSC]
repeatFor range st =  do
  gen <- updateGen
  concat <$> replicateM (fst $ randomR range gen) st

r00 :: State StdGen [OSC]
r00 = concat <$> sequence [r01,r02,r03]

r01 :: State StdGen [OSC]
r01 = do
  gen <- updateGen
  let notes = fst $ chooseOne [[], [24,31,36,43,48,55]] gen
  return $ map new_help_SPE1 notes

r02 :: State StdGen [OSC]
r02 = repeatFor (2,5) r02' 

r03 :: State StdGen [OSC]
r03 = repeatFor (3,9) r03'

r02' :: State StdGen [OSC]
r02' = do
  gen <- updateGen
  let notes = 60 : fst (chooseOne [63,65] gen) : 67 : 
              fst (chooseOne [70,72,74] gen) : []
  return $ map new_help_SPE1 notes

r03' :: State StdGen [OSC]
r03' = do
  gen <- updateGen
  let notes = fst $ chooseOne [74,75,77,79,81] gen
  return $ map new_help_SPE1 [notes]

