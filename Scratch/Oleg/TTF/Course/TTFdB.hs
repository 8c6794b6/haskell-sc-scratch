{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Lecture: <http://okmij.org/ftp/tagless-final/course/TTFdB.hs>
-}
module TTFdB where

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

------------------------------------------------------------------------------
-- Interpreter to evaluate

newtype R h a = R { unR :: h -> a }

instance Symantics R where
  int x     = R $ const x
  add e1 e2 = R $ \h -> (unR e1 h) + (unR e2 h)
  
  z         = R $ \(x,_) -> x
  s v       = R $ \(_,h) -> unR v h
  lam e     = R $ \h -> \x -> unR e (x,h)
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

eval e = unR e ()

td1_eval = eval td1

td2_eval = eval td2
td2_eval' = eval td2 21

-- td3_eval :: (Int -> Int) -> Int
td3_eval = eval td3

------------------------------------------------------------------------------
-- Interpreter to show

newtype S h a = S { unS :: Int -> String }

instance Symantics S where
  int x = S $ const $ show x 
  add e1 e2 = S $ \h -> 
    "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"
  z = S $ \h -> "x" ++ show (h-1)
  s v = S $ \h -> unS v (h-1)
  lam e = S $ \h ->
    let x = "x" ++ show h
    in  "(\\" ++ x ++ " -> " ++ unS e (h+1) ++ ")"
  app e1 e2 = S $ \h ->
    "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"
    
view e = unS e 0

class MulSYM repr where
  mul :: repr h Int -> repr h Int -> repr h Int
  
instance MulSYM R where  
  mul e1 e2 = R $ \h -> unR e1 h * unR e2 h
  
instance MulSYM S where  
  mul e1 e2 = S $ \h -> "(" ++ unS e1 h ++ "*" ++ unS e2 h ++ ")"
  
tf4 = mul (int 3) (int 4)

tf4_eval = eval tf4

tf5 = lam (add tf4 z)

-- tf5_eval :: Int -> Int
tf5_eval = eval tf5

class BoolSYM repr where
  bool :: Bool -> repr r Bool
  if_ :: repr r Bool -> repr r a -> repr r a -> repr r a
  leq :: repr r Int -> repr r Int -> repr r Bool
  
instance BoolSYM R where  
  bool b = R $ const b
  if_ p e1 e2 = R $ \h -> if unR p h then unR e1 h else unR e2 h
  leq e1 e2 = R $ \h -> unR e1 h <= unR e2 h
  
instance BoolSYM S where  
  bool b = S $ \h -> show b 
  if_ p e1 e2 = S $ \h -> 
    "if " ++ unS p h ++ " then " ++ unS e1 h ++ " else " ++ unS e2 h
  leq e1 e2 = S $ \h -> 
    "leq (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  
tf6 = lam (if_ (leq z (int 0)) (int 0) (int 999))

-- tf6_eval :: Int -> Int
tf6_eval = eval tf6

class FixSYM repr where
  fix :: repr (a,h) a -> repr h a
  
instance FixSYM R where  
  fix e = R $ \h -> let a = unR e (a,h) in unR e (a,h)
        
instance FixSYM S where        
  fix e = S $ \h -> "fix " ++ unS e h

powfix () = 
  lam {- x -} (
    fix {- self -} (
       lam {- n -} (
         let n = z; self = s z; x = s (s z)  
         in  if_ (leq n (int 0)) (int 1)
                  (mul x (app self (add n (int (-1))))))))
  
powfix7 = lam (app (app (powfix ()) z) (int 7))
powfix72 = app powfix7 (int 2)

powfix210 = app (app (powfix ()) (int 2)) (int 10)