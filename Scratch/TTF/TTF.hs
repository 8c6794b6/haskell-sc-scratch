{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Lecture: <http://okmij.org/ftp/tagless-final/course/TTF.hs>
--
-- HOAS, Higher Order Abstract Syntax.
--
module TTF where

-- | Core language
class Symantics repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
  
  lam :: (repr a -> repr b) -> repr (a->b)
  app :: repr (a->b) -> repr a -> repr b
  
th1 = add (int 1) (int 2)  

th2 = lam (\x -> add x x)

th3 = lam (\x -> add (app x (int 1)) (int 2))

-- | Typed and tagless interpreter
newtype R a = R {unR :: a}

instance Symantics R where
  int x     = R x
  add e1 e2 = R $ unR e1 + unR e2
  
  lam f     = R $ unR . f . R
  app e1 e2 = R $ (unR e1) (unR e2)
  
-- lam' f = R $ \x -> unR (f (R x))

eval e = unR e

type VarCounter = Int
newtype S a = S {unS :: VarCounter -> String}

instance Symantics S where
  int x = S $ const $ show x
  add e1 e2 = S $ \h -> "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"
  
  lam e     = S $ \h ->
    let x = "x" ++ show h
    in  "(\\" ++ x ++ " -> " ++ unS (e (S $ const x)) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"
  
view e = unS e 0  
           
class MulSYM repr where  
  mul :: repr Int -> repr Int -> repr Int
  
class BoolSYM repr where
  bool :: Bool -> repr Bool
  leq :: repr Int -> repr Int -> repr Bool
  if_ :: repr Bool -> repr a -> repr a -> repr a
  
class FixSYM repr where  
  fix :: (repr a -> repr a) -> repr a
  
tpow = lam (\x -> fix (\self -> lam (\n ->  
                                      if_ (leq n (int 0)) (int 1)
                                          (mul x (app self (add n (int (-1))))))))
       
tpow7 = lam (\x -> app (app tpow x) (int 7))       

tpow72 = app tpow7 (int 2)

instance MulSYM R where
  mul e1 e2 = R $ unR e1 * unR e2
  
instance BoolSYM R where  
  bool b = R b
  leq e1 e2 = R $ unR e1 <= unR e2
  if_ be et ee = R $ if unR be then unR et else unR ee
  
instance FixSYM R where
  fix f = R $ fx (unR . f . R) where fx f = f (fx f)
                                     
instance MulSYM S where
  mul e1 e2 = S $ \h ->
    "(" ++ unS e1 h ++ "*" ++ unS e2 h ++ ")"
    
instance BoolSYM S where
  bool x = S $ const $ show x
  leq e1 e2 = S $ \h -> 
    "(" ++ unS e1 h ++ "<=" ++ unS e2 h ++ ")"
  if_ be et ee = S $ \h ->  
    unwords ["(if", unS be h, "then", unS et h, "else", unS ee h, ")"]
    
instance FixSYM S where    
  fix e = S $ \h ->
    let self = "self" ++ show h
    in  "(fix " ++ self ++ "." ++ 
        unS (e (S $ const self)) (succ h) ++ ")"
