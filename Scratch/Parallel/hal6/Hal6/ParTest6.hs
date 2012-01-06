{-# LANGUAGE BangPatterns #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (GHC specific)

Parallel test, take 2. Looking for max with summing up every elements.
Chunking the list before creating sparks. Using mapReduce with threshold.
-}
module Hal6.ParTest6 where

import Control.Parallel.Strategies
import Data.List (foldl1')
import Hal6.Collatz

main :: IO ()
main = print $ partest 500000

mapReduce ::
  Int -> -- ^ Threshold
  Int -> -- ^ Lower bound
  Int -> -- ^ Upper bound
  Strategy a -> -- ^ Strategy
  (Int -> a) -> -- ^ Function to be mapped
  ([a] -> a) -> -- ^ Function used while reducing
  a
mapReduce n lo hi s f c = runEval $ go lo hi where
  go lo' hi'
    | m < n     = rpar `dot` s $ c (map f [lo'..hi'])
    | otherwise = do
      r1 <- go lo'    m2
      r2 <- go (m2+1) hi'
      return $ c [r1, r2]
    where
      m = hi' - lo'
      m2 = lo' + m `div` 2

combine :: (Int, Int) -> (Int, Int) -> (Int, Int)
combine (!m1, !s1) (!m2, !s2) = (m1 `max` m2, s1+s2)

partest :: Int -> (Int, Int)
partest n =
  mapReduce threshold 1 n rdeepseq
  ((\x -> (x,x)) . collatz . fromIntegral) (foldl1' combine)
  where
    threshold = 1000
