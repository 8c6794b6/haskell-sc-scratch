{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 6, variation b.

-}
module Take06b where

import Type01

class Sym r h where
  v   :: h a -> r h a
  lam :: (h a -> r h b) -> r h (a->b)
  app :: r h (a->b) -> r h a -> r h b

  int :: Int -> r h Int
  add :: r h Int -> r h Int -> r h Int

e01 = add (int 1) (int 2)
e02 = lam (\x -> v x)
e03 = lam (\x -> add (v x) (int 3))
e04 = lam (\x -> lam (\y -> add (v x) (v y)))
e05 = app (app e04 (int 5)) (int 6)
e06 = lam (\x -> app (v x) (int 9))
e07 = app e06 e03

newtype R (h :: * -> *) a = R {unR :: a}

newtype Id a = Id {unId :: a}

instance Sym R Id where
  v (Id x) = R x
  lam f = R $ \x -> unR (f (Id x))
  app e1 e2 = R $ (unR e1) (unR e2)

  int x = R x
  add e1 e2 = R $ unR e1 + unR e2

eval :: R Id a -> a
eval = unR

newtype S (h :: * -> *) a = S {unS :: Int -> String}

newtype VarCounter a = VarCounter Int

toS :: S VarCounter a -> S VarCounter a
toS = id

instance Sym S VarCounter where
  v (VarCounter x) = S $ \_ -> "v x" ++ show x
  lam f = S $ \h ->
    let x = "x" ++ show h
    in  "lam (\\" ++ x ++ " -> " ++ unS (f (VarCounter h)) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  int x = S $ const $ "int " ++ show x
  add e1 e2 = S $ \h -> "add (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

view :: S VarCounter a -> String
view e = unS e 0

instance Show (S VarCounter a) where
  show = view

data VarIdx = VZ | VS VarIdx

instance Show VarIdx where
  show VZ = "VZ"
  show (VS v) = "VS (" ++ shows v ")"

varI :: VarIdx -> Int
varI v = case v of VZ -> 0; VS v' -> succ (varI v')

newtype DeBruijn a = DeBruijn VarIdx

instance Show (DeBruijn a) where
  show (DeBruijn i) = show i

newtype D (h :: * -> *) a = D {unD :: VarIdx -> String}

instance Sym D DeBruijn where
  v (DeBruijn i) = D $ \_ -> "v x" ++ show (varI i)
  lam f = D $ \h ->
    let x = "x" ++ show (varI h)
    in  "lam (\\" ++ x ++ " -> " ++ unD (f (DeBruijn h)) (VS h) ++ ")"

  app e1 e2 = D $ \h -> "app (" ++ unD e1 h ++ ") (" ++ unD e2 h ++ ")"

  int x = D $ \h -> "int " ++ show x
  add e1 e2 = D $ \h -> "add (" ++ unD e1 h ++ ") (" ++ unD e2 h ++ ")"

toD :: D DeBruijn a -> D DeBruijn a
toD = id

dbj :: D DeBruijn a -> String
dbj e = unD e VZ

instance Show (D DeBruijn a) where
  show = dbj

newtype E (h :: * -> *) a = E {unE :: Int -> String}