{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading typefun.pdf.
Bench marking memo function for Bool table.
-}
module Scratch03 where

import Criterion.Main
import Scratch01 (Memo(..),fibonacci,factorial)

main = defaultMain
  [ bench "without memo" (nf (map (\x -> f x + f x)) [True,False])
  , bench "with memo" (nf (map (\x -> g x + g x)) [True,False])
  ]

g :: Bool -> Integer
g = fromTable (toTable f)

f :: Bool -> Integer
f p = if p then factorial 100 else fibonacci 30
