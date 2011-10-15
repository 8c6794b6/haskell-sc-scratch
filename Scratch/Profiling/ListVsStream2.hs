{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Comparing between standard list and fusion stream.

-}
module ListVsStream2 where

import Prelude hiding (mapM_)

import Criterion.Main
import Control.Monad.Stream
import Data.List.Stream

import qualified Data.Vector.Fusion.Stream as V

main :: IO ()
main = defaultMain
  [ bench "list"
    (whnf (mapM_ print) (enumFromTo 1 10000))
    -- When compiled, stream fusion is much faster
    -- When interpreted in ghci, not so much difference.
  , bench "fusion stream"
    (whnf (V.mapM_ print) (V.enumFromStepN 1 1 10000))]