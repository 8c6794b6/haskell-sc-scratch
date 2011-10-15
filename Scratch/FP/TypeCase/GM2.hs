{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

-}
module GM2 where

import Prelude hiding (sum)

import LIGD hiding (TypeRep(..))

class Generic g where
  unit :: g Unit
  char :: g Char
  int  :: g Int
  prod :: g a -> g b -> g (Prod a b)
  sum  :: g a -> g b -> g (Sum a b)
  datatype :: (b :<-> a) -> g a -> g b

class FunctorRep f where
  functorRep :: Generic g => g a -> f (g a)

newtype Count a = Count {applyCount :: a -> Int}

instance Generic Count where
  unit = Count $ \_ -> 0
  char = Count $ \_ -> 1
  int = Count $ \_ -> 1
  sum a b = Count $ \x -> case x of
    Inl l -> applyCount a l
    Inr r -> applyCount b r
  prod a b = Count $ \(Prod x y) -> applyCount a x + applyCount b y
  datatype iso a = Count $ \x -> applyCount a (from iso x)

-- gSize :: FunctorRep f => f a -> Int
-- gSize = applyCount (functorRep (Count (\_ -> 1)))
