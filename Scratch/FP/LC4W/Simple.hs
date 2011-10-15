{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

From 'lambda-calculus cooked four ways'.
-}
module Simple (nf) where

import Data.List (union, (\\))

import Lambda
import IdInt

nf :: LC IdInt -> LC IdInt
nf e@(Var _) = e
nf (Lam x e) = Lam x (nf e)
nf (App f a) = case whnf f of
  Lam x b -> nf (subst x a b)
  f'      -> App (nf f') (nf a)

whnf :: LC IdInt -> LC IdInt
whnf e@(Var _) = e
whnf e@(Lam _ _) = e
whnf (App f a) = case whnf f of
  Lam x b -> whnf (subst x a b)
  f'      -> App f' a

subst :: IdInt -> LC IdInt -> LC IdInt -> LC IdInt
subst x s b = sub b where
  sub e@(Var v) | v == x    = s
                | otherwise = e
  sub e@(Lam v e') | v == x = e
                   | v `elem` fvs = Lam v' (sub e'')
                   | otherwise    = Lam v (sub e') where
                     v' = newId vs
                     e'' = subst v (Var v') e'
  sub (App f a) = App (sub f) (sub a)
  fvs = freeVars s
  vs = fvs `union` allVars b

newId :: [IdInt] -> IdInt
newId vs = head ([firstBoundId .. ] \\ vs)
