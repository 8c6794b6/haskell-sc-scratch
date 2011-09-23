{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading jfp paper of typed tagless finall interpreter.

-}
module Scratch01 where

------------------------------------------------------------------------------
-- Translation of OCaml code in /1.1 The tag problem/.

data Var = VZ | VS Var

data Exp 
  = V Var
  | B Bool
  | L Exp
  | A Exp Exp
    
lkup :: [e] -> Var -> e
lkup [] _     = error "Unbound variable"
lkup (x:es) v = case v of
  VZ    -> x
  VS v' -> lkup es v'

data U = UB Bool | UA (U -> U)

instance Show U where
  show (UB b) = "UB " ++ show b
  show (UA _) = "UA <fun>"

eval :: [U] -> Exp -> U
eval env e = case e of
  V v -> lkup env v
  B b -> UB b
  L f -> UA (\x -> eval (x:env) f)
  A e1 e2 -> case eval env e1 of
    UA f -> f (eval env e2)
    
test1 = A (L (V VZ)) (B True)    
test2 = A (B True) (B False)

eval_test1 = eval [] test1
-- Shows:
-- UB True

------------------------------------------------------------------------------
-- /1.3. Our final proposal/

z :: (a,x) -> a
z (a,_) = a

s :: (a->b) -> (h,a) -> b
s f (_,h) = f h

lam :: ((a,h) -> b) -> h -> a -> b
lam e env = \x -> e (x,env)

app :: (h -> (a->b)) -> (h->a) -> (h->b)
app e1 e2 env = (e1 env) (e2 env)

b :: Bool -> a -> Bool
b bv env = bv

testF1 :: h -> Bool
testF1 = app (lam z) (b True)

evalF :: (() -> t) -> t
evalF f = f ()

evalF_testF1 = evalF testF1

-- Type checker of higher order object language
-- 
-- * Guillemette and Monnier,  2006
-- * Pasalic et al., 2002
-- * Baars and Swierstra, 2002