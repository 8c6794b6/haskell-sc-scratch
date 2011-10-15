{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 2, variation b.

-}
module Take02b where

import Type01

class Sym r where
  int :: Int -> r Int
  add :: r Int -> r Int -> r Int
  lam :: (r a -> r b) -> r (a->b)
  app :: r (a->b) -> r a -> r b
  var :: a -> r a

e01 = add (int 1) (int 2)
e02 = lam (\x -> x)
e03 = app e02 (int 100)
e04 = lam (\x -> lam (\y -> add x y))
e05 = app (app e04 (int 2)) (int 3)
e06 = lam (\x -> app x (int 3))
e07 = app e06 e02

newtype R a = R {unR :: a}

instance Sym R where
  int = R
  add e1 e2 = R $ unR e1 + unR e2
  var = R
  lam f = R $ \x -> unR $ f (R x)
  app e1 e2 = R $ (unR e1) (unR e2)

eval :: R a -> a
eval = unR

newtype S a = S {unS :: Int -> String}

toS :: S a -> S a
toS = id

instance Sym S where
  int x = S $ const $ "int " ++ show x
  add e1 e2 = S $ \h -> "add (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  var _ = S $ \h -> 'x' : show h
  lam f = S $ \h ->
    let x = show h
    in  "lam (\\x" ++ x ++ " -> " ++ unS (f (S $ const $ 'x':x)) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

view :: S a -> String
view e = unS e 0

instance Show (S a) where
  show = view
