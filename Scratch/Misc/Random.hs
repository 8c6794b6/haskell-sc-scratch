{-# LANGUAGE BangPatterns #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Playing with random.

-}
module Random where

import Control.Monad
import Data.List (unfoldr)

import Control.Monad.ST (ST, runST)
import Control.Monad.Primitive (PrimState,PrimMonad)
import System.Random.MWC

rs :: Int -> IO [Double]
rs n = go n where
  go x | x == 0    = return []
       | otherwise = do
    r <- withSystemRandom (uniform :: GenIO -> IO Double)
    (r:) `fmap` go (x-1)

rs2 n = replicateM n (withSystemRandom (uniform :: GenIO -> IO Double))

rs3 :: Int -> IO ()
rs3 n = withSystemRandom (go n) where
  go :: Int -> GenIO -> IO ()
  go x seed
    | x == 0 = return ()
    | otherwise = do
      r <- (uniform :: GenIO -> IO Double) seed
      print r
      go (pred x) seed

go4 :: (PrimMonad m, Functor m) => Int -> Gen (PrimState m) -> m [Int]
go4 x seed
  | x == 0 = return []
  | otherwise = do
    r <- uniformR (0,100::Int) seed
    (r:) `fmap` go4 (pred x) seed

go5 x = sequence $ unfoldr f 0 where
  f b | b == x = Nothing
      | otherwise = Just (return b,succ b)

go6 x seed
  | x == 0    = return []
  | otherwise = do
    r <- uniformR (0,100) seed
    (r:) `fmap` go6 (pred x) seed

u :: ST s Double
u = do
  let last = 1000000 :: Int
  gen <- create
  let loop !n !i | n == last = return i
                 | otherwise = uniform gen >>= loop (n+1)
  loop 0 0

goU = print (runST u)

r01 = do
  gen <- create
  d1 <- uniformR (0,1) =<< create
  d2 <- uniformR (0,1) =<< create
  mapM_ print [d1 :: Double,d2]
