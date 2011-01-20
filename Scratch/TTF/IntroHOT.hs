------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Lecture: <http://okmij.org/ftp/tagless-final/course/PushNegFI.hs>
--
module IntroHOT where

data Var = VZ | VS Var

data Exp = V Var
         | B Bool
         | L Exp
         | A Exp Exp
           
lookp VZ (x:_) = x
lookp (VS v) (_:env) = lookp v env

data U = UB Bool | UA (U -> U)

instance Show U where
  show (UB x) = "UB " ++ show x
  show (UA _) = "UA <fun>"
  
eval :: [U] -> Exp -> U
eval env exp = case exp of
  V v     -> lookp v env
  B b     -> UB b
  L e     -> UA (\x -> eval (x:env) e)
  A e1 e2 -> case eval env e1 of
    UA f -> f (eval env e2)
                    
ti1 = A (L (V VZ)) (B True)

-- partial pattern match in the A clause
ti2a = A (B True) (B False)

-- partial pattern match in lookp
ti2o = A (L (V (VS VZ))) (B True)