{-# LANGUAGE
  FunctionalDependencies
, FlexibleInstances
, FlexibleContexts
, GADTs
, KindSignatures
, MultiParamTypeClasses
, UndecidableInstances
, TypeFamilies
 #-}
{-
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* <http://byorgey.wordpress.com/2010/06/29/typed-type-level-programming-in-haskell-part-i-functional-dependencies/>

-}
module Brent01 where

{-
Example shown in first post of type-level programming.
-}

data Z
data S n

class Plus m n r | m n -> r
instance Plus Z n n
instance (Plus m n r) => Plus (S m) n (S r)

a :: Plus (S Z) (S Z) r => r
a = undefined

-- ghci> :t a
-- a :: S (S Z)

{-
Rewriting above with type families
-}

-- data Z2
-- data S2 n

type family Res a b :: *

type instance Res Z n = n
type instance Res (S m) n = S (Res m n)

b :: r ~ Res (S Z) (S Z) => r
b = undefined

b' :: Res (S Z) (S Z)
b' = undefined

-- ghci> :t b
-- b :: S (S Z)

-- ghci> :t b'
-- b' :: Res (S Z) (S Z)

{-
Rewriting with data families
-}

data family Res2 a b :: *

data instance Res2 Z n = ZRes
data instance Res2 (S m) n = SRes (Res2 m n)

c :: r ~ Res2 (S Z) (S Z) => r
c = undefined

c' = SRes (SRes ZRes)

-- ghci> :t c'
-- c' :: Res2 (S (S Z)) n

-- ghci> :t c
-- c :: Res2 (S Z) (S Z)

------------------------------------------------------------------------------
-- From type-level programming III.

data LOL :: * -> * -> * where
  KThxBye :: LOL Z a
  Moar    :: a -> LOL n a -> LOL (S n) a

append :: LOL m a -> LOL n a -> LOL (Res m n) a
append KThxBye v = v
append (Moar x xs) v = Moar x (append xs v)

data Nat = Zero | Suc Nat

type family Plus3 m n :: *
type instance Plus3 Z n = n
type instance Plus3 (S m) n = S (Plus3 m n)
