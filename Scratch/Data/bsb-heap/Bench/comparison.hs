{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Control.DeepSeq (NFData(..))
import Data.List (sort)
import System.Random

import Data.BsbHeap
import Data.BsbHeap.SbHeap (SbHeap(..), Tree(..))
import qualified Data.BsbHeap.SbHeap as Sb

import Bench.Util

main :: IO ()
main = do
  g <- newStdGen
  let n = 50000
      k = 1000
      (e,_) = randomR (1, maxBound::Int) g
  defaultMain
    [ bgroup "find minimum (nf)"
        [ bench "list" (nf minimum (mkList n g))
        , bench "skew heap" (nf Sb.findMin (mkSkew n g))
        , bench "boot heap" (nf findMin (mkBoot n g))
        ]
    , bgroup "sort (whnf)"
        [ bench "list" (whnf sort (mkList n g))
        , bench "skew heap" (whnf Sb.toSortedList (mkSkew n g))
        , bench "boot heap" (whnf toSortedList (mkBoot n g))
        ]
    , bgroup "add element (nf)"
        [ bench "list" (nf (e:) (mkList n g))
        , bench "skew heap" (nf (Sb.insert e) (mkSkew n g))
        , bench "boot heap" (nf (insert e) (mkBoot n g))
        ]
    , bgroup "insert and sort (nfIO)"
        [ bench "list" (nfIO ((sort . mkList k) `fmap` newStdGen))
        , bench "skew heap" (nfIO ((Sb.toSortedList . mkSkew k) `fmap` newStdGen))
        , bench "boot heap" (nfIO ((toSortedList . mkBoot k) `fmap` newStdGen))
        ]
    ]
