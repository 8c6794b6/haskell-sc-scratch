{-# LANGUAGE
  FunctionalDependencies
, FlexibleInstances
, MultiParamTypeClasses
, TypeFamilies
, TypeOperators
, nUndecidableInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

UA - Ultra Applicative

From: <http://okmij.org/ftp/Haskell/TypeLambdaVal.hs>

-}
module UA where

data HTrue
data HFalse
data HBool x

hTrue :: HBool HTrue
hTrue = undefined

hFalse :: HBool HFalse
hFalse = undefined

data Nil = Nil
data x :* xs = HCons x xs
infixr 5 :*

newtype HList x = HList x

nil :: HList Nil
nil = HList Nil

infixr 5 *:
(*:) :: x -> HList xs -> HList (x :* xs)
x *: (HList xs) = HList (HCons x xs)

hHead :: HList (x :* xs) -> x
hHead (HList (HCons x _)) = x

hTail :: HList (x :* xs) -> HList xs
hTail (HList (HCons _ xs)) = HList xs

-- Data constructor HList, HCons, and Nil should not be used after this point.

------------------------------------------------------------------------------
-- Work horse

class Apply f x res | f x -> res where
  apply :: f -> x -> res

instance a ~ a' => Apply (a -> b) a' b where
  apply = ($)
