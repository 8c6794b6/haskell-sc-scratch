{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6, 2011
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Benchmark for comparing FFT with repa-fftw to repa-algorithms.
-}
module Main where

import Data.Complex
import System.Random

import Criterion.Main

import Data.Array.Repa ((:.)(..), Z(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Algorithms.FFT as A
import qualified Data.Array.Repa.FFTW as F

main :: IO ()
main = do
  rs <- randomRs (0,1) `fmap` newStdGen
  is <- randomRs (0,1) `fmap` newStdGen
  let bench_fft n =
        let mkarr ks = R.fromList (Z:.n) $ take n ks
            ts = mkarr $ zip rs is
            cs = mkarr $ zipWith (:+) rs is
        in  ts `seq` cs `seq`
            bgroup ("n="++show n)
              [ bench "repa-algorithms" (whnf (A.fft1d A.Forward) ts)
              , bench "repa-fftw" (whnf F.fft cs) ]
  defaultMain $ map (\k -> bench_fft (2^k)) [9..13::Int]
