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
import Data.List (sort)
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import qualified Data.BsbHeap as B
import qualified Data.BsbHeap.SbHeap as K
import Bench.Util

main :: IO ()
main = do
  g <- newStdGen
  let n = 50000
  -- mapM_ print $ F.toList $ Seq.unstableSort $ mkSeq n g
  mapM_ print $ B.toSortedList $ mkBoot n g
  -- mapM_ print $ K.toSortedList $ mkSkew n g
  -- mapM_ print $ sort $ mkList n g

      -- e = K.findMin $ mkSkew n g
      -- e = Set.findMin $ mkSet n g
      -- e = B.findMin $ mkBoot n g
  -- putStrLn $ "Min element in heap: " ++ show e
