------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook07
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook06_Phrase_Network/.
--
-- Reimplementing with performance tuning in mind.
--

module SCHelp.PG.Cookbook072 where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq, (|>), (<|))
import System.Random
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Sequence as S

import FRP.Reactive
import Sound.OpenSoundControl
import Sound.SC3

import Reusable hiding ((|>))
import SCQuery
import SCSched
import SCTree

import SCHelp.PG.Cookbook07 (kik, kraftySnr)

runRhythms :: IO ()
runRhythms = undefined

data RhythmState = RhythmState 
    { rhythmCurrent :: Int,
      rhythmGen :: StdGen }

isLast4 :: Int -> Bool
isLast4 n = 12 < n' && n' <= 16 where n' = n `mod` 16

getValues :: String -> Map String [Double] -> [Double]
getValues k m = maybe [] id . M.lookup k $ m

initState :: IO RhythmState
initState = RhythmState 0 <$> newStdGen

hhRhythms :: IO (Event OSC)
hhRhythms = do
  m <- hhEvent 128
  let durs = scanl (+) 0 $ getValues "dur" m
      oscs = mkSNew' "kraftySnr" 1 m
  return $ listE $ zip durs oscs

hhEvent :: Int -> IO (Map String [Double])
hhEvent n = evalState (M.unionsWith (++) <$> replicateM n hhDelta) <$> initState

hhBase :: Map String [Double]
hhBase = M.fromList $
         [("amp", replicate 16 15),
          ("dur", replicate 16 0.25),
          ("rq", repeat 0.06),
          ("decay", repeat 0.04),
          ("freq", repeat 12000)]

hhDelta :: State RhythmState (Map String [Double])
hhDelta = do
  RhythmState current gen <- get
  let (_, gen') = next gen
  put $ RhythmState (current + 4) gen'
  if isLast4 current 
     then return (last4HH gen)
     else return (other4HH gen)

hhPart :: RandomGen g => (Int,Int) -> g -> Map String [Double]
hhPart range g = M.update (updateHHAmp idx) "amp" .
                 M.update (updateHHDur idx) "dur" $ hhBase
    where
      idx = take (fst $ randomR range g) (fst $ shuffle [0..15] g)

last4HH :: RandomGen g => g -> Map String [Double]
last4HH = hhPart (2, 5) 

other4HH :: RandomGen g => g -> Map String [Double]
other4HH = hhPart (0, 2)

updateHHAmp :: [Int] -> [Double] -> Maybe [Double]
updateHHAmp idx vals = updateHHVal [15, 10] idx vals

updateHHDur :: [Int] -> [Double] -> Maybe [Double]
updateHHDur idx vals = updateHHVal [0.125,0.125] idx vals

updateHHVal :: (Ord k, Monad m, Enum k, Num k) => 
               [a] -> [k] -> [a] -> m [a]
updateHHVal val idx vals = return vals'
    where
      vals' = concat $ M.elems $ foldr f v idx
      v = M.fromList $ zip [0..] (map return vals)
      f a m = M.update (\_ -> return val) a m

snrRhythms = undefined

kikRhythms = undefined