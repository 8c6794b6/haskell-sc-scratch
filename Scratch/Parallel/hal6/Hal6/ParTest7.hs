{-# LANGUAGE BangPatterns #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (GHC specific)

Parallel test, take 2. Looking for max with summing up every elements.
Chunking the list before creating sparks. Using mapReduce with threshold.
Threshold would be determined from number of available cores.

-}
module Hal6.ParTest7 where

import Control.Parallel.Strategies
import Data.List (foldl1')
import GHC.Conc
import Hal6.Collatz
import Hal6.ParTest6 (mapReduce, combine)

main :: IO ()
main = print $ partest 500000

autoMapReduce :: Int -> Int -> Strategy a -> (Int->a) -> ([a]->a) -> a
autoMapReduce lo hi = mapReduce ((hi-lo) `div` (numCapabilities*5)) lo hi

partest :: Int -> (Int, Int)
partest n = autoMapReduce 1 n rdeepseq f g where
  f = (\x -> (x,x)) . collatz . fromIntegral
  g = foldl1' combine
