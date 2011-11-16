{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import System.Random
import Data.List (sort)

import Data.BsbHeap

main :: IO ()
main = do
  g <- newStdGen
  let n = 50000
      k = 5000
  defaultMain
    [ bgroup "sort"
        [bench "list" (whnf sort (mkList n g))
        , bench "heap" (whnf toSortedList (mkHeap n g))
        ]
    , bgroup "bare insert"
        [ bench "list" (nfIO (mkList k `fmap` newStdGen))
        , bench "heap" (nfIO (mkHeap k `fmap` newStdGen))
        ]
    , bgroup "insert and sort"
        [ bench "list" (nfIO ((sort . mkList k) `fmap` newStdGen))
        , bench "heap" (nfIO ((toSortedList . mkHeap k) `fmap` newStdGen))
        ]
    ]

mkList :: Int -> StdGen -> [Int]
mkList n g = go n g [] where
  go !k !gen acc = case k of
    0 -> acc
    _ -> case randomR (0,maxBound::Int) gen of
      (!x,!gen') -> go (k-1) gen' (x:acc)

mkHeap :: Int -> StdGen -> Heap Int
mkHeap n g = go n g empty where
  go !k !gen acc = case k of
    0 -> acc
    _ -> case randomR (0,maxBound::Int) gen of
      (!x,!gen') -> go (k-1) gen' (insert x acc)
