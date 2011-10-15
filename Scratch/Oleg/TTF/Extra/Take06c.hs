{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 6, variation c.

-}
module Take06c where

class Sym r where
  v   :: V a -> r a
  lam :: (V a -> r b) -> r (a->b)
  app :: r (a->b) -> r a -> r b

  int :: Int -> r Int
  add :: r Int -> r Int -> r Int

data V a = VZ | VS (V a)

instance Show (V a) where
  show VZ = "VZ"
  show (VS x) = "VS (" ++ shows x ")"

t01 = lam (\x -> v x)
t02 = lam (\x -> lam (\y -> app (v x) (v y)))
t03 = add (int 1) (int 2)

newtype S a = S {unS :: V a -> String}

data Vw = forall a. Vw (V a)

instance Sym S where
  v x = S $ \_ -> show x

  -- lam f = S $ \h ->
  --   let x = "x" ++ show h
  --   in  "lam (\\" ++ x ++ " -> " ++ unS (f $ S (const $ show h)) (VS h) ++ ")"

  -- app e1 e2 = S $ \h ->
  --   "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

  lam = undefined
  app = undefined

  int x = S $ \_ -> "int " ++ show x
  add e1 e2 = S $ \h -> "add (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

view :: S h -> String
view e = unS e VZ

data L a where
  Nil  :: L ()
  Cons :: a -> L b -> L (a,b)

data VI = SV VI | ZV deriving (Show)

class Idx g where
  type Value g :: *
  idx :: VI -> L g -> Value g

instance Idx () where
  type Value () = ()
  idx _ Nil = ()

instance Idx g => Idx (t,g) where
  type Value (t,g) = Either (Value g) t
  idx ZV (Cons e _)      = Right e
  idx (SV v) (Cons _ es) = Left $ idx v es

l1 = Cons 'a' (Cons True (Cons [1,2,3] (Cons (False,'b',Just [4,5]) Nil)))
e1 = idx ZV l1
e2 = idx (SV ZV) l1
e3 = idx (SV (SV ZV)) l1
