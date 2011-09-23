{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 4.

-}
module Take04 where

------------------------------------------------------------------------------
-- Symantic class, with 'v' variable lifter.

class Sym r where
  int :: Int -> r Int
  add :: r Int -> r Int -> r Int
  
  v   :: a -> r a
  lam :: (a -> r b) -> r (a->b)
  app :: r (a->b) -> r a -> r b
  
ue01 = add (int 3) (int 4)
ue02 = lam (\x -> v x)
ue03 = lam (\x -> add (v x) (v x))
ue04 = app ue03 (int 8)
ue05 = lam (\x -> lam (\y -> add (v x) (v y)))

------------------------------------------------------------------------------
-- Evaluation interpreter

newtype R a = R {unR :: a}  

instance Sym R where
  int e = R e
  add e1 e2 = R $ unR e1 + unR e2
  
  v x = R $ x
  lam f = R $ \x -> unR (f x)
  app e1 e2 = R $ (unR e1) (unR e2)

eval :: R a -> a
eval = unR

newtype VarCounter a = VarCounter Int

newtype S h a = S {unS :: Int -> String}

instance Sym (S Int) where
  int e = S $ const $ show e
  add e1 e2 = S $ \h -> unS e1 h ++ " + " ++ unS e2 h
  
  -- XXX: Got stuck.
  -- Cannot increase the var counter passed to argument of lam, function f.
  -- We want to distinguish the counter in v, to keep the index number of 
  -- variable in expression.
  --
  v _ = S $ \h -> "v x" ++ show h
  lam f = S $ \h ->
    let x = "x" ++ show (succ h)
    in  "lam (\\" ++ x ++ " -> " ++ unS (f undefined) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  
view :: S Int a -> String
view e = unS e 0

