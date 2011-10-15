{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 5, variation 5.

-}
module Take05b where

class Sym r h where
  v :: h a -> r h a
  lam :: (h a -> r h b) -> r h (a->b)
  app :: r h (a->b) -> r h a -> r h b

  int :: Int -> r h Int
  add :: r h Int -> r h Int -> r h Int

e01 = add (int 1) (int 2)
e02 = lam (\x -> v x)
e03 = lam (\x -> add (v x) (int 5))
e04 = app e03 (int 8)
e05 = lam (\x -> lam (\y -> add (v x) (v y)))
e06 = app (app e05 (int 24)) (int 75)
e07 = lam (\x -> app (v x) (int 8))
e08 = app e07 e03

data VarIdx a = VZ | VS (VarIdx a)

newtype R a = R {unR :: VarEnv -> a}

data VarList where
  Nil  :: VarList
  Cons :: t -> VarList -> VarList

class VarEnv g where
  type Value g :: *
  findvar :: VarIdx -> g -> Value g

-- instance Sym R VarIdx where
--   v x = case x of
--     VZ ->
