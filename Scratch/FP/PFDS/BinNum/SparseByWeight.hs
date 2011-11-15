{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains /sparse/ representation of binary numbers.
/Sparse/ means, the representation does not contain zero.

-}
module BinNum.SparseByWeight where

type Nat = [Int]

carry :: Int -> Nat -> Nat
carry w ws = case ws of
  []     -> [w]
  w':rest | w < w'    -> w:ws
          | otherwise -> carry (2*w) rest

borrow :: Int -> Nat -> Nat
borrow w ws@(w':rest)
  | w == w'   = rest
  | otherwise = w : borrow (2*w) ws

inc :: Nat -> Nat
inc = carry 1

dec :: Nat -> Nat
dec = borrow 1

add :: Nat -> Nat -> Nat
add xs ys = case (xs,ys) of
  (_,[]) -> xs
  ([],_) -> ys
  (w1:ws1,w2:ws2)
    | w1 < w2   -> w1 : add ws1 ys
    | w1 > w2   -> w2 : add xs ws2
    | otherwise -> carry (2*w1) (add ws1 ws2)

n2i :: Nat -> Int
n2i = sum

i2n :: Int -> Nat
i2n = (iterate inc [] !!)
