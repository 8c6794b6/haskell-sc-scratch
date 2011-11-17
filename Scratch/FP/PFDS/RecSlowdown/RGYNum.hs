{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This module contains lazy recursive slowdown number, described in chapter 8.

-}
module BinNum.RGYNum where

import Control.DeepSeq (NFData(..))
import Criterion.Main

data Digit
  = One
  | Two
  | Three
  deriving (Show)

d2i :: Digit -> Int
d2i d = case d of One -> 1; Two -> 2; Three -> 3

type Nat = [Digit]

inc :: Nat -> Nat
inc ns = case ns of
  []          -> [One]
  (One:ns')   -> Two:ns'
  (Two:ns')   -> Three:ns'
  (Three:ns') -> Two:inc ns'

dec :: Nat -> Nat
dec ns = case ns of
  One:[] -> []
  One:ns' -> Two:dec ns'
  Two:ns' -> One:ns'
  Three:ns' -> Two:ns'

n2i :: Nat -> Int
n2i ns = go 0 ns where
  go k ds = case ds of
    []     -> 0
    (d:ds') -> (2^k * (d2i d)) + go (k+1) ds'

i2n :: Int -> Nat
i2n = (iterate inc [] !!)

test_n2i :: [Int]
test_n2i = 
  let ns = iterate inc []
  in  [n2i (ns !! x) | x <- [1..100]]
