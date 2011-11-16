{-# LANGUAGE BangPatterns #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Bench for profiling memory usage.

-}
module Main where

import System.Random (StdGen, randomR, newStdGen)
import Data.BsbHeap
import Data.List (sort)
import qualified Data.Foldable as F
import qualified Data.Sequence as S

main :: IO ()
main = do
  g <- newStdGen
  let n = 50000
  -- mapM_ print $ F.toList $ S.unstableSort $ mkSeq n g
  mapM_ print $ toSortedList $ mkHeap n g
  -- mapM_ print $ sort $ mkList n g

mkHeap :: Int -> StdGen -> Heap Int
mkHeap n g = go n g empty where
  go !k !gen acc = case k of
    0 -> acc
    _ -> case randomR (0,maxBound::Int) gen of
      (!x,!gen') -> go (k-1) gen' (insert x acc)

mkList :: Int -> StdGen -> [Int]
mkList n g = go n g [] where
  go !k !gen acc = case k of
    0 -> acc
    _ -> case randomR (0,maxBound::Int) gen of
      (!x,!gen') -> go (k-1) gen' (x:acc)

mkSeq :: Int -> StdGen -> S.Seq Int
mkSeq n g = go n g S.empty where
  go !k !gen acc = case k of
    0 -> acc
    _ -> case randomR (0,maxBound::Int) gen of
      (!x,!gen') -> go (k-1) gen' (x S.<| acc)
