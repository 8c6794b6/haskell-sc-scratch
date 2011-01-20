------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Lecture: <http://okmij.org/ftp/tagless-final/course/PushNegI.hs>
--
-- What we want to do is, change the grammer from:
-- 
-- > e ::= lit | neg e | add e e
-- 
-- to: 
--
-- > e ::= factor | add e e
-- > factor ::= lit | neg lit
--
-- Only integer literals can be negated, and only once.
-- 
module PushNegI where

import Intro1

push_neg :: Exp -> Exp
push_neg e = case e of
  Lit _           -> e
  Neg (Lit _)     -> e
  Neg (Neg e')    -> e'
  Neg (Add e1 e2) -> Add (push_neg (Neg e1)) (push_neg (Neg e2))
  Add e1 e2       -> Add (push_neg e1) (push_neg e2)

ti1_norm_view = view $ push_neg ti1


