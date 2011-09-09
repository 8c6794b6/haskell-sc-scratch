{-# LANGUAGE GADTs #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Lecture: <http://okmij.org/ftp/tagless-final/course/IntroHOIF.hs>
-}
module IntroHOIF where

-- Initial

data Var env t where
  VZ :: Var (t,env) t
  VS :: Var env t -> Var (a,env) t
  
data Exp env t where
  B :: Bool -> Exp env Bool
  V :: Var env t -> Exp env t
  L :: Exp (a,env) b -> Exp env (a->b)
  A :: Exp env (a->b) -> Exp env a -> Exp env b
  
lookp :: Var env t -> env -> t
lookp VZ (x,_) = x
lookp (VS v) (_,env) = lookp v env

eval :: env -> Exp env t -> t
eval env (V v) = lookp v env
eval env (B b) = b
eval env (L e) = \x -> eval (x,env) e
eval env (A e1 e2) = (eval env e1) (eval env e2)

ti1 = A (L (V VZ)) (B True)
ti1_eval = eval () ti1

ti2 = (A (A (L (L (V (VS VZ)))) (B True)) (B False))
ti2_eval = eval () ti2

ti2o = A (L (V (VS VZ))) (B True)

-- Final

vz (vc,_) = vc
vs vp (_,envr) = vp envr

b bv _ = bv
l e env = \x -> e (x,env)
a e1 e2 env = (e1 env) (e2 env)

tf1 = a (l vz) (b True)
tf1_eval = tf1 ()

tf2 = (a (a (l (l (vs vz))) (b True)) (b False))
tf2_eval = tf2 ()

tf2o = a (l (vs vz)) (b True)
