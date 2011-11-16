{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This module contains skew binary number, described in chapter 6.4.

Skew binary numbers:

   * Weight: w(i) = 2 ^ (i+1) - 1
   * Digits: D(i) = {0, 1, 2}

92 (in decimal) = 002101 (in skew binary numbers)

> 002101 = 0 * (2^1-1) -- 0
>        + 0 * (2^2-1) -- 0
>        + 2 * (2^3-1) -- 14
>        + 1 * (2^4-1) -- 15
>        + 0 * (2^5-1) -- 0
>        + 1 * (2^6-1) -- 63

Only the lowest non-0 digit may be 2, to avoid redundancy.

-}
module BinNum.Skew where

type Nat = [Int]

inc :: Nat -> Nat
inc ss = case ss of
  (w1:w2:rest)
    | w1 == w2  -> 1+w1+w2 : rest
    | otherwise -> 1 : ss
  _ -> 1 : ss

dec :: Nat -> Nat
dec ss = case ss of
  1:ws -> ws
  w:ws -> case w `div` 2 of w' -> w':w':ws

n2i :: Nat -> Int
n2i = sum

i2n :: Int -> Nat
i2n = (iterate inc [] !!)