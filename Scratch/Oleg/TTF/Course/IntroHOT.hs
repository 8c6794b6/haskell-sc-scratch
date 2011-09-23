{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Lecture: <http://okmij.org/ftp/tagless-final/course/IntroHOT.hs>
-}
module IntroHOT where

data Var = VZ | VS Var

data Exp
  = V Var
  | B Bool
  | L Exp
  | A Exp Exp
    
lookp VZ (x:_) = x
lookp (VS v) (_:env) = lookp v env

data U = UB Bool | UA (U -> U)

instance Show U where
  show (UB x) = "UB " ++ show x
  show (UA _) = "UA <fun>"
  
eval env (V v) = lookp v env  
eval env (B b) = UB b
eval env (L e) = UA (\x -> eval (x:env) e)
eval env (A e1 e2) = case eval env e1 of UA f -> f (eval env e2)
                                         
ti1 = A (L (V VZ)) (B True)                                         
ti1_eval = eval [] ti1

-- Partial pattern match in A clause.
-- Permitting invalid syntax.
ti2a = A (B True) (B False)
ti2a_eval = eval [] ti2a
                   
-- Open term.
-- We can get stucked when we evaluate an open term.
ti2o = A (L (V (VS VZ))) (B True)
ti2o_eval = eval [] ti2o