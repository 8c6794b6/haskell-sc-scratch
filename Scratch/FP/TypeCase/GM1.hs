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
module GM1 where

import Prelude hiding (sum)

import LIGD hiding (TypeRep(..))

class Generic g where
  unit :: g Unit
  sum  :: (TypeRep a,TypeRep b) => g (Sum a b)
  prod :: (TypeRep a,TypeRep b) => g (Prod a b)
  datatype :: TypeRep a => (b :<-> a) -> g b
  char :: g Char
  int  :: g Int

class TypeRep a where
  typeRep :: Generic g => g a

instance TypeRep Unit where
  typeRep = unit

instance TypeRep Char where
  typeRep = char

instance TypeRep Int where
  typeRep = int

instance (TypeRep a,TypeRep b) => TypeRep (Sum a b) where
  typeRep = sum

instance (TypeRep a,TypeRep b) => TypeRep (Prod a b) where
  typeRep = prod

instance (TypeRep a,TypeRep b) => TypeRep (a,b) where
  typeRep = datatype (EP f t) where
    f (x,y) = Prod x y
    t (Prod x y) = (x,y)

instance TypeRep a => TypeRep (Maybe a) where
  typeRep = datatype (EP f t) where
    f x = case x of
      Nothing -> Inl Unit
      Just x  -> Inr x
    t x = case x of
      Inl Unit -> Nothing
      Inr x    -> Just x

instance TypeRep a => TypeRep [a] where
  typeRep = datatype (EP f t) where
    f bs = case bs of
      []     -> Inl Unit
      (x:xs) -> Inr (Prod x xs)
    t x = case x of
      Inl Unit        -> []
      Inr (Prod x xs) -> x:xs

newtype GSize a = GSize {appGSize :: a -> Int}

instance Generic GSize where
  unit = GSize $ \_ -> 0
  sum = GSize $ \t -> case t of Inl x -> gSize x; Inr y -> gSize y
  prod = GSize $ \(Prod x y) -> gSize x + gSize y
  datatype iso = GSize $ \t -> gSize (from iso t)
  char = GSize $ \_ -> 1
  int = GSize $ \_ -> 1

gSize :: TypeRep a => a -> Int
gSize = appGSize typeRep