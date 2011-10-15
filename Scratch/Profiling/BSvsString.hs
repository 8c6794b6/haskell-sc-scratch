{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Comparing between ByteString and String Eq operation.

-}
module BSvsString where

import Criterion.Main
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main =
  let ls = ["bar", "buzz", "fool", "fo", "foo", "fook", "f"]
  in  defaultMain
      [ bench "string" (whnf (map (== "foo")) ls)
      , bench "bytestring" (whnf (map (== (C8.pack "foo"))) (map C8.pack ls))
      ]