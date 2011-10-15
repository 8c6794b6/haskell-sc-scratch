{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:
/Practical type inference for arbitrary-rank types/.

-}
module Scratch02 where

f1 :: (forall a. [a]->Int) -> Int
f1 x =
  let a1 = x [1,2,3]
      a2 = x [True,False]
      a3 = x "foo"
  in  a1 + a2 + a3

-- ghci> f1 length
-- 18

data Any = forall a. Any a

f2 :: ([Any] -> Int) -> Int
f2 x =
  let a1 = x [Any 1, Any 2, Any 3]
      a2 = x [Any True, Any False]
      a3 = x [Any 'f', Any 'o', Any 'o']
  in  a1 + a2 + a3

-- ghci> f2 length
-- 8
