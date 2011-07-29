{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Scratch written while reading GADT page in haskell wiki, from 
-- <http://www.haskell.org/haskellwiki/GADT>.
-- 
module HaskellWiki where

{-

Motivating example: Writing SK calculus.
 
In functional expression, k combinator is operationally:

> k = \x y -> x 

and s combinator is:

> s = \x y z -> x z (y z) 

Which could be expressed with  type signature as:

> k :: a -> b -> a
> s :: (a -> b -> c) -> (a -> b) -> a -> c

-}

data Term0 = K0 | S0 | Term0 :@- Term0
infixl 6 :@-

data Term x where
  K     :: Term (a -> b -> a)
  S     :: Term ((a -> b -> c) -> (a -> b) -> a -> c)
  Const :: a -> Term a
  (:@)  :: Term (a -> b) -> Term a -> Term b
  
infixl 6 :@

instance Show x => Show (Term x) where
  show (Const a) = show a
  show _        = "<function>"

eval :: Term a -> Term a
eval (K :@ x :@ y) = x
eval (S :@ x :@ y :@ z) = x :@ z :@ (y :@ z)
eval x = x

