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
import Data.Monoid (mconcat)
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

import SCHelp.PG.Cookbook07 (kik, kraftySnr, setRhythmicVariations)

runRhythms :: Int -> IO ()
runRhythms n = do
  hh <- hhRhythms n
  snr <- snrRhythms n
  kik <- kikRhythms n
  spawn 0 128 $ mconcat $ [hh, snr, kik]

data RhythmState = RhythmState
    { rhythmCurrent :: Int,
      rhythmGen :: StdGen }

initState :: IO RhythmState
initState = RhythmState 0 <$> newStdGen

isLast4 :: Int -> Bool
isLast4 n = 12 <= n' && n' < 16 where n' = n `mod` 16

getValues :: String -> Map String [Double] -> [Double]
getValues k m = maybe [] id . M.lookup k $ m

getRestIndices :: Map String [Double] -> [Int]
getRestIndices m = map fst . filter (\(a,b) -> b == 0) . 
                   zip [0..] . getValues "amp" $ m

rhythmDelta :: (StdGen -> a) -> (StdGen -> a) -> State RhythmState a
rhythmDelta f1 f2 = do
  RhythmState current gen <- get
  let (_, gen') = next gen
  put $ RhythmState (current+4) gen'
  if isLast4 current
     then return $ f1 gen
     else return $ f2 gen

mkEvent :: Int 
        -> State RhythmState (Map String [Double]) 
        -> IO (Map String [Double])
mkEvent n st = 
    evalState (M.unionsWith (++) <$> replicateM n st) <$>
    initState

hhRhythms :: Int -> IO (Event OSC)
hhRhythms n = do
  m <- mkEvent n hhDelta
  let durs = scanl (+) 0 $ getValues "dur" m
      oscs = mkSNew' "kraftySnr" 1 m
  return $ listE $ zip durs oscs

snrRhythms :: Int -> IO (Event OSC)
snrRhythms n = do
  m <- mkEvent n $ snrDelta
  let durs = scanl (+) 0 $ getValues "dur" m
      oscs = mkSNew' "kraftySnr" 1 m
  return $ listE $ zip durs oscs

kikRhythms :: Int -> IO (Event OSC)
kikRhythms n = do
  m <- mkEvent n kikDelta
  let durs = scanl (+) 0 $ getValues "dur" m
      oscs = mkSNew' "kik" 1 m
  return $ listE $ zip durs oscs

hhBase :: Map String [Double]
hhBase = M.fromList $
         [("amp", replicate 16 15),
          ("dur", replicate 16 0.25),
          ("rq", repeat 0.06),
          ("decay", repeat 0.04),
          ("freq", repeat 12000)]

snrBase :: Map String [Double]
snrBase = M.fromList $ 
          [("amp", [0,0,0,0, 1,0,0,0, 0,0,0,0, 0.7,0,0,0]),
           ("decay", [0,0,0,0, 0.35,0,0,0, 0,0,0,0, 0.2,0,0,0]),
           ("dur", replicate 16 0.25),
           ("freq", replicate 16 5000)]

kikBase :: Map String [Double]
kikBase = M.fromList $ 
          [("amp", [1,0,0,0, 0,0,0.7,0, 0,1,0,0, 0,0,0,0]),
           ("decay", [0.15,0,0,0, 0,0,0.15,0, 0,0.15,0,0, 0,0,0,0]),
           ("preamp", replicate 16 0.4),
           ("dur", replicate 16 0.25)]

hhDelta :: State RhythmState (Map String [Double])
hhDelta = rhythmDelta (hhPart (2,5)) (hhPart (0,2))

snrDelta :: State RhythmState (Map String [Double])
snrDelta = rhythmDelta (snrPart (5,9)) (snrPart (1,3))

kikDelta :: State RhythmState (Map String [Double])
kikDelta = rhythmDelta (kikPart (5,10)) (kikPart (0,2))

snrPart :: RandomGen g => (Int, Int) -> g -> Map String [Double]
snrPart range g = M.update (updateSnrVal (0.20, 0.40) idx g) "amp" .
                  M.update (updateSnrVal (0.15, 0.30) idx g) "decay" $
                  snrBase
    where
      idx = take (fst $ randomR range g) 
            (fst $ shuffle (getRestIndices snrBase) g)

kikPart :: RandomGen g => (Int, Int) -> g -> Map String [Double]
kikPart range g = M.update (updateSnrVal (0.2, 0.5) idx g) "amp" .
                  M.update (updateSnrVal (0.05, 0.10) idx g) "decay" $
                  kikBase
    where
      idx = take (fst $ randomR range g) 
            (fst $ shuffle (getRestIndices kikBase) g)


updateSnrVal :: RandomGen g => (Double,Double) -> [Int] -> g ->
                [Double] -> Maybe [Double]
updateSnrVal range idx g vals = return vals'
    where
      vals' = M.elems $ foldr f v idx
      v = M.fromList $ zip [0..] vals
      f a b = M.update (\_ -> return $ fst $ randomR range g) a b


hhPart :: RandomGen g => (Int,Int) -> g -> Map String [Double]
hhPart range g = M.update (updateHHVal [15,10] idx) "amp" .
                 M.update (updateHHVal [0.125,0.125] idx) "dur" $ hhBase
    where
      idx = take (fst $ randomR range g) (fst $ shuffle [0..15] g)

updateHHVal :: (Ord k, Monad m, Enum k, Num k) =>
               [a] -> [k] -> [a] -> m [a]
updateHHVal val idx vals = return vals'
    where
      vals' = concat $ M.elems $ foldr f v idx
      v = M.fromList $ zip [0..] (map return vals)
      f a m = M.update (\_ -> return val) a m
