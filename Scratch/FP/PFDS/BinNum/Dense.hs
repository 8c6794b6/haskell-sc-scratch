{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains /dense/ representation of binary numbers.
/Dense/ means, the representation contains zero.

-}
module BinNum.Dense where

data Digit = Zero | One deriving Show

-- | Natural numbers, in dense format.
type Nat = [Digit]

-- | 0.
zero :: Nat
zero = [Zero]

-- | Convert from Nat to Int.
n2i :: Nat -> Int
n2i ns = go 0 ns where
  go :: Int -> Nat -> Int
  go x ns = case ns of
    []         -> 0
    (Zero:ns') -> 0 + go (x+1) ns'
    (One:ns')  -> (2^x) + go (x+1) ns'

-- | Convert from Int to Nat.
i2n :: Int -> Nat
i2n = (iterate inc zero !!)

{-# RULES "round_trip/n2n"  forall n. i2n (n2i n) = n #-}
{-# RULES "round_trip/i2i"  forall i. n2i (i2n i) = i #-}

three :: Int
three = n2i (i2n 3)

inc :: Nat -> Nat
inc ns = case ns of
  [] -> [One]
  (Zero:ns') -> One : ns'
  (One:ns')  -> Zero : inc ns'

dec :: Nat -> Nat
dec ns = case ns of
  [One]      -> []
  (One:ns')  -> Zero:ns'
  (Zero:ns') -> One:dec ns'

add :: Nat -> Nat -> Nat
add xs ys = case (xs,ys) of
  (_,[]) -> xs
  ([],_) -> ys
  (d:ds1, Zero:ds2) -> d : add ds1 ds2
  (Zero:ds1, d:ds2) -> d : add ds1 ds2
  (One:ds1, One:ds2) -> Zero : inc (add ds1 ds2) -- carry