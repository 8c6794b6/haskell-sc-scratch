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
-- Lecture: <http://okmij.org/ftp/tagless-final/course/TTFdB.hs>
--
module TTFdB where

-- s and z could been removed to class Var var_repr.
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

td3 = lam (add (app z (int 1)) (int 2))

newtype R h a = R {unR :: h -> a}

instance Symantics R where
  int x     = R $ \_ -> x
  add e1 e2 = R $ \h -> (unR e1 h) + (unR e2 h)

  z = R $ \(x,_) -> x
  s v = R $ \(_,h) -> unR v h
  lam e = R $ \h -> \x -> unR e (x,h)
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

eval e = unR e ()

newtype S h a = S {unS :: [String] -> String}

instance Symantics S where
  int x     = S $ \_ -> show x
  add e1 e2 = S $ \h ->
    "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"

  z = S $ \(x:_) -> x
  s v = S $ \(_:h) -> unS v h
  lam e = S $ \h ->
    let x = "x" ++ show (length h)
    in  "(\\" ++ x ++ " -> " ++ unS e (x:h) ++ ")"
  app e1 e2 = S $ \h ->
    "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

view :: S () a -> String
view e = unS e []

--
-- Exercises
--

class MulSYM repr where
  mul :: repr r Int -> repr r Int -> repr r Int

instance MulSYM R where
  mul e1 e2 = R $ \h -> (unR e1 h) * (unR e2 h)

instance MulSYM S where
  mul e1 e2 = S $ \h -> "(" ++ unS e1 h ++ "*" ++ unS e2 h ++ ")"

tdm1 = mul (int 3) (int 4)

tdm2 = lam (mul z z)

tdm3 = lam (mul (app z (int 3)) (int 2))

class BoolSYM repr where
  bool :: Bool -> repr r Bool
  if_ :: repr r Bool -> repr r a -> repr r a -> repr r a
  leq :: repr r Int -> repr r Int -> repr r Bool

instance BoolSYM R where
  bool b = R $ \_ -> b
  if_ p e1 e2 = R $ \h -> if unR p h then unR e1 h else unR e2 h
  leq a b = R $ \h -> unR a h <= unR b h

instance BoolSYM S where
  bool b = S $ \_ -> show b
  if_ p e1 e2 = S $ \h ->
    "if " ++ unS p h ++ " then " ++ unS e1 h ++ " else " ++ unS e2 h
  leq a b = S $ \h -> unS a h ++ " <= " ++ unS b h

-- Not yet implemented.
class FixSYM repr where
  fix :: repr (a,h) a -> repr h a

-- Another interpreter that interprets each term to give its size
-- (the number of constructors)