{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable (NoMonomorphismRestriction)

De brujin Higher order tagless typed final exercise.
-}
module DeBruijn where

class Symantics repr where
  int :: Int -> repr h Int
  add :: repr h Int -> repr h Int -> repr h Int
  z   :: repr (a,h) a
  s   :: repr h a -> repr (any,h) a
  lam :: repr (a,h) b -> repr h (a->b)
  app :: repr h (a->b) -> repr h a -> repr h b

td1 = add (int 1) (int 2)
td2 = lam (add z z)

td2o = lam (add z (s z))
-- td2o :: Symantics repr => repr (Int, h) (Int -> Int)
-- td2o needs a environment with Int

td3 = lam (add (app z (int 1)) (int 2))
-- td3 :: Symantics repr => repr h ((Int -> Int) -> Int)

newtype R h a = R {unR :: h -> a}

instance Symantics R where
  int x = R $ \_ -> x
  add e1 e2 = R $ \h -> unR e1 h + unR e2 h
  z = R $ \(a,_) -> a
  s v = R $ \(_,h) -> unR v h
  lam e = R $ \h -> \a -> unR e (a,h)
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

eval e = unR e ()

newtype S h a = S {unS :: Int -> String}

view :: S () a -> String
view e = unS e 0

instance Symantics S where
  int x = S $ \_ -> show x
  add e1 e2 = S $ \h -> '(' : unS e1 h ++ " + " ++ unS e2 h ++ ")"
  z = S $ \h -> "x" ++ show (h-1)
  s v = S $ \h -> unS v (h-1)
  lam e = S $ \h ->
    let x = "x" ++ show h
    in  "(\\" ++ x ++ " -> " ++ unS e (h+1) ++ ")"
  app e1 e2 = S $ \h ->
    '(' : unS e1 h ++ " " ++ unS e2 h ++ ")"

-- ghci> putStrLn $ view $ lam td2o
-- (\x0 -> (\x1 -> (x1 + x0)))

------------------------------------------------------------------------------
-- Mul

class MulSYM repr where
  mul :: repr r Int -> repr r Int -> repr r Int

tm1 = mul (int 25) (int 4)
tm2 = lam (mul z z)
tm3 = app (lam (mul z (int 2))) (int 50)
-- tm3 :: (MulSYM repr, Symantics repr) => repr h Int

tm4 = lam (mul (app z (int 2)) (int 50))
-- tm4 :: (MulSYM repr, Symantics repr) => repr h ((Int -> Int) -> Int)

instance MulSYM R where
  mul e1 e2 = R $ \h -> unR e1 h * unR e2 h

instance MulSYM S where
  mul e1 e2 = S $ \h -> '(':unS e1 h ++ " * " ++ unS e2 h ++ ")"

------------------------------------------------------------------------------
-- Bool

class BoolSYM repr where
  bool :: Bool -> repr r Bool
  if_ :: repr r Bool -> repr r a -> repr r a -> repr r a
  leq :: repr r Int -> repr r Int -> repr r Bool

tb1 = bool True
tb2 = lam (if_ (leq (int 0) z) z (int 0))

instance BoolSYM R where
  bool b = R $ \_ -> b
  if_ p t f = R $ \h ->
    let p' = unR p h
    in if p' then unR t h else unR f h
  leq e1 e2 = R $ \h -> unR e1 h <= unR e2 h

instance BoolSYM S where
  bool b = S $ \_ -> show b
  if_ p t f = S $ \h ->
    "if (" ++ unS p h ++ ") then (" ++ unS t h ++ ") else (" ++ unS f h ++ ")"
  leq e1 e2 = S $ \h -> unS e1 h ++ " <= " ++ unS e2 h

------------------------------------------------------------------------------
-- Fix

class FixSYM repr where
  fix :: repr (a,h) a -> repr h a

tpow =
  lam (fix (lam (
               let n = z; self = s z; x = s (s z)
               in  if_ (leq n (int 0)) (int 1)
                   (mul x (app self (add n (int (-1))))))))
-- ghci> eval tpow 2 7
-- 128

tpow7 = app tpow (int 7)
tpow27 = app (app tpow (int 2)) (int 7)

instance FixSYM R where
  fix e = R $ \h -> let a = unR e (a,h) in a

instance FixSYM S where
  fix e = S $ \h ->
    let self = "x" ++ show h
    in  "(fix " ++ self ++ "." ++ unS e (h+1) ++ ")"

main = do
  let k = putStrLn . view
  k td1
  k td2
  k td3
  k tm1
  k tm2
  k tm3
  k tm4
  k tb1
  k tb2
  k tpow
  k tpow7
  k tpow27