{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/tagless-final/Interp.hs>
-}
module Interp where

data Z = Z deriving Show
data S a = S a deriving Show

class Nat n
instance Nat Z
instance Nat a => Nat (S a)

data TInt = TInt deriving Show
data TArr t1 t2 = TArr t1 t2 deriving Show

class Typ t
instance Typ TInt
instance (Typ t1, Typ t2) => Typ (TArr t1 t2)

newtype B = B Int deriving Show
newtype V v = V v deriving Show
data L typ exp = L typ exp deriving Show
data A e1 e2 = A e1 e2 deriving Show

class Exp e
instance Exp B
instance Nat v => Exp (V v)
instance (Typ typ, Exp exp) => Exp (L typ exp)
instance (Exp e1, Exp e2) => Exp (A e1 e2)

data HNil = HNil deriving Show
data HCons a b = HCons a b deriving Show

class Typ t => TypEval t r | t -> r
instance TypEval TInt Int
instance (Typ t1, Typ t2, TypEval t1 r1, TypEval t2 r2)
         => TypEval (TArr t1 t2) (r1->r2)

class Eval gamma exp result | gamma exp -> result where
  eval :: exp -> gamma -> result

instance Eval gamma B Int where
  eval (B i) _ = i

instance Eval (HCons v g) (V Z) v where
  eval _ (HCons v _) = v

instance Eval g (V n) r => Eval (HCons v g) (V (S n)) r where
  eval (V (S n)) (HCons _ g) = eval (V n) g

instance (TypEval t tr, Eval (HCons tr g) e r)
         => Eval g (L t e) (tr->r) where
  eval (L _ exp) g = \x -> eval exp (HCons x g)

instance (Eval g e1 (a->r), Eval g e2 a)
         => Eval g (A e1 e2) r where
  eval (A e1 e2) g = (eval e1 g) (eval e2 g)


data Add e1 e2 = Add e1 e2 deriving Show
instance (Eval g e1 Int, Eval g e2 Int)
         => Eval g (Add e1 e2) Int where
  eval (Add e1 e2) g = eval e1 g + eval e2 g

evalNil e = eval e HNil

t1 = L TInt (B 1)
t2 = A (L TInt (B 1)) (B 2)
t3 = A (L TInt (V Z)) (B 2)
t4 = A (L (TArr TInt TInt) (V Z))

u1 = A (L TInt (Add (V Z) (B 1))) (B 10)