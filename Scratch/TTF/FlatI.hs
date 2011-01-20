{-# LANGUAGE FlexibleInstances #-}
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
-- Lecture: <http://okmij.org/ftp/tagless-final/course/FlatI.hs>
--
-- The goal is to convert the addition tree to the right-skewed form
-- 
-- * (A + B) + R => A + (B + R)
--
-- Previouslly, expressions were constructed according to this grammer:
--
-- > e ::= lit | neg e | add e e
-- 
-- Restricted grammer now:
--
-- > e ::= factor | add factor e
-- > factor ::= lit | neg lit
--
-- Only integer literals can be negated, and only once.
--
module FlatI where

import Intro1 
import PushNegI as Neg

flata :: Exp -> Exp
flata e = case e of
  Lit _              -> e
  Neg _              -> e
  Add (Add e1 e2) e3 -> flata (Add e1 (Add e2 e3))
  Add e1 e2          -> Add e1 (flata e2)

norm :: Exp -> Exp
norm = flata . push_neg

ti3 = (Add ti1 (Neg (Neg ti1)))