{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Control.DeepSeq (NFData(..))
import Data.List (sort)
import System.Random

import qualified Data.BsbHeap as B
import qualified Data.BsbHeap.SbHeap as Sb

import Bench.Util

main :: IO ()
main = do
  g <- newStdGen
  let n = 50000
      k = 1000
      (e,_) = randomR (1, maxBound::Int) g
  defaultMain
    [ bgroup "list, skew, bootstrapped"
      [ bgroup "find minimum (nf)"
        [ bench "list" (nf minimum (mkList n g))
        , bench "skew" (nf Sb.findMin (mkSkew n g))
        , bench "boot" (nf B.findMin (mkBoot n g))
        ]
      , bgroup "sort (whnf)"
        [ bench "list" (whnf sort (mkList n g))
        , bench "skew" (whnf Sb.toSortedList (mkSkew n g))
        , bench "boot" (whnf B.toSortedList (mkBoot n g))
        ]
      , bgroup "add element (nf)"
        [ bench "list" (nf (e:) (mkList n g))
        , bench "skew" (nf (Sb.insert e) (mkSkew n g))
        , bench "boot" (nf (B.insert e) (mkBoot n g))
        ]
      , bgroup "insert and sort (nfIO)"
        [ bench "list" (nfIO ((sort . mkList k) `fmap` newStdGen))
        , bench "skew"
          (nfIO ((Sb.toSortedList . mkSkew k) `fmap` newStdGen))
        , bench "boot"
          (nfIO ((B.toSortedList . mkBoot k) `fmap` newStdGen))
        ]
      ]
    , bgroup "changing n"
      [ let skew n = bench ("skew n=" ++ show n) (nf Sb.findMin (mkSkew n g))
            boot n = bench ("boot n=" ++ show n) (nf B.findMin (mkBoot n g))
        in  bgroup "find minimum, boot (nf)"
            [ skew (10^3), skew (10^4), skew (10^5)
            , boot (10^3), boot (10^4), boot (10^5) ]
      , let skew n = bench ("skew n=" ++ show n) (nf (Sb.insert e) (mkSkew n g))
            boot n = bench ("boot n=" ++ show n) (nf (B.insert e) (mkBoot n g))
        in  bgroup "add element, boot (nf)"
            [ skew (10^3), skew (10^4), skew (10^5)
            , boot (10^3), boot (10^4), boot (10^5) ]
      ]
    ]
