{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains /segmented dense/ representation of binary numbers.
/Dense/ means, the representation contains zero.
/Segmented means, sequence of same elements are grouped in block.

-}
module BinNum.SegDense where

-- | Represent segment of binary number.
data DigitBlock = Zeros Int | Ones Int deriving Show

-- | Representation of  natural number with binary nums.
type Nat = [DigitBlock]

zeros :: Int -> Nat -> Nat
zeros i bs = case (i,bs) of
  (_,[])             -> []
  (_,Zeros j : rest) -> Zeros (i+j) : rest
  (0, _)             -> bs
  _                  -> Zeros i : bs

ones :: Int -> Nat -> Nat
ones i bs = case (i,bs) of
  (_,Ones j:rest) -> Ones (i+j) : rest
  (0,_)           -> bs
  _               -> Ones i : bs

inc :: Nat -> Nat
inc ns = case ns of
  [] -> [Ones 1]
  Zeros i : rest -> ones 1 (zeros (i-1) rest)
  Ones i : rest  -> Zeros i : inc rest

dec :: Nat -> Nat
dec ns = case ns of
  [] -> []
  Ones i : rest  -> zeros 1 (ones (i-1) rest)
  Zeros i : rest -> Ones i : dec rest

------------------------------------------------------------------------------
-- Tests

n2i :: Nat -> Int
n2i ns = go 0 ns where
  go j bs = case bs of
    []    -> 0
    b:bs' -> case b of
      Zeros i -> go (j+i) bs'
      Ones i  -> 2^(j+i) - 2^j + go (j+i) bs'

i2n :: Int -> Nat
i2n = (iterate inc [] !!)

