{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Parallel test, take 1.
-}
module Hal6.ParTest1 where

import Control.Parallel.Strategies
import Hal6.Collatz hiding (maxC)

main :: IO ()
main = print $ partest 500000

maxC :: Int -> Int -> Int
maxC lo hi =
  let cs = map (collatz . fromIntegral) [lo..hi]
  in  maximum cs

partest :: Int -> Int
partest n = runEval $ do
  let h = n `div` 2
  r1 <- rpar $ maxC 1     h
  r2 <- rpar $ maxC (h+1) n
  -- let c (m1, s1) (m2, s2) = (m1 `max` m2, s1 + s2)
  return $ r1 `max` r2
