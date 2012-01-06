{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Function used in parallel exercise.

-}

module Hal6.Collatz where

collatz :: Integer -> Int
collatz n = case n of
  0 -> 0
  1 -> 0
  _  | even n    -> 1 + collatz (n `div` 2)
     | otherwise -> 1 + collatz (3 * n + 1)

maxC :: Int -> Int -> Int
maxC lo hi = maximum [collatz $ fromIntegral x | x<-[lo..hi]]

maxCP :: Int -> Int -> (Int, Int)
maxCP lo hi =
  let cs = map (collatz . fromIntegral) [lo..hi]
  in  (maximum cs, sum cs)
