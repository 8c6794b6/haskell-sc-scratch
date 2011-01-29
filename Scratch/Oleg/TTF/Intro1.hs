------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Lecture: <http://okmij.org/ftp/tagless-final/course/Intro1.hs>
--
-- Expression need to express an integer algebra like:
--
-- > 8 + (- (1+2))
--
module Intro1 where

data Exp = Lit Int
         | Neg Exp
         | Add Exp Exp

ti1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

eval :: Exp -> Int
eval (Lit n)     = n
eval (Neg e)     = - (eval e)
eval (Add e1 e2) = eval e1 + eval e2

type Repr = Int

lit :: Int -> Repr
lit n = n

neg :: Repr -> Repr
neg e = - e

add e1 e2 = e1 + e2

tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))

view :: Exp -> String
view (Lit n) = show n
view (Neg e) = "(-" ++ view e ++ ")"
view (Add e1 e2) = "(" ++ view e1 ++ " + " ++ view e2 ++ ")"