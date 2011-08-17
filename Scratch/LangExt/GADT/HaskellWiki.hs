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

import Control.Monad

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

data Parser tok a where
  Zero :: Parser tok ()
  One :: Parser tok ()
  Check :: (tok -> Bool) -> Parser tok tok
  Satisfy :: ([tok] -> Bool) -> Parser tok [tok]
  Push :: tok -> Parser tok a ->  Parser tok a
  Plus :: Parser tok a -> Parser tok b -> Parser tok (Either a b)
  Times :: Parser tok a -> Parser tok b -> Parser tok (a,b)
  Star :: Parser tok a -> Parser tok [a]

parse :: Parser tok a -> [tok] -> Maybe a
parse Zero _ = mzero
parse One _ = Just ()

parse (Check p) ts = case ts of
  [t] -> if p t then return t else mzero
  _   -> mzero

parse (Satisfy p) xs = if p xs then return xs else mzero

parse (Push t x) ts = parse x (t:ts)

parse (Plus x y) ts = fmap Left (parse x ts) `mplus` fmap Right (parse y ts)

parse (Times x y) [] = liftM2 (,) (parse x []) (parse y [])
parse (Times x y) (t:ts) =
  parse (Times (Push t x) y) ts `mplus`
  liftM2 (,) (parse x []) (parse y (t:ts))

parse (Star x) [] = return []
parse (Star x) (t:ts) = do
  (v,vs) <- parse (Times x (Star x)) (t:ts)
  return (v:vs)

token x = Check (== x)
string xs = Satisfy (== xs)

p1 = Times (token 'a') (token 'b')
p2 = Times (Star (token 'a')) (Star (token 'b'))
p3 = Star p2

blocks :: (Eq tok) => Parser tok [[tok]]
blocks = Star (Satisfy allEqual) where
  allEqual xs = and (zipWith (==) xs (tail xs))

evenOdd = Plus (Star (Times (Check even) (Check odd)))
          (Star (Times (Check odd) (Check even)))