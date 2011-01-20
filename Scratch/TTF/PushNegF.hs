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
-- Lecture: <http://okmij.org/ftp/tagless-final/course/PushNegF.hs>
--
-- The nested pattern matching in PushNegI establishes a context,
-- defined as Ctx.
--
module PushNegF where

import Intro2

data Ctx = Pos | Neg

instance (ExpSYM repr) => ExpSYM (Ctx -> repr) where
  lit n = \c -> case c of
    Pos -> lit n
    Neg -> neg (lit n)
  neg e = \c -> case c of
    Pos -> e Neg
    Neg -> e Pos
  add e1 e2 = \c -> add (e1 c) (e2 c)

push_neg :: (Ctx -> t) -> t
push_neg e = e Pos

tf1_norm = push_neg tf1