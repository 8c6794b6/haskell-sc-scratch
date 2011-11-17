{-# LANGUAGE BangPatterns #-}
module Bench.Util where

import System.Random

import Data.BsbHeap
import Data.BsbHeap.SbHeap (SbHeap(..), Tree(..))
import qualified Data.BsbHeap.SbHeap as Sb
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

mkList :: Int -> StdGen -> [Int]
mkList = mkRand [] (:)

mkBoot :: Int -> StdGen -> Heap Int
mkBoot = mkRand empty insert

mkSkew :: Int -> StdGen -> SbHeap Int
mkSkew = mkRand Sb.empty Sb.insert

mkSeq :: Int -> StdGen -> Seq.Seq Int
mkSeq = mkRand Seq.empty (Seq.<|)

mkSet :: Int -> StdGen -> Set.Set Int
mkSet = mkRand Set.empty (Set.insert)

mkRand :: t -> (Int -> t -> t) -> Int -> StdGen -> t
mkRand acc incr n g = go n g acc where
  go !k !gen acc = case k of
    0 -> acc
    _ -> case randomR (0,maxBound::Int) gen of
      (!x,!gen') -> go (k-1) gen' (incr x acc)
