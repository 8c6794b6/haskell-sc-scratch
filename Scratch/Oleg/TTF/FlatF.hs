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
-- The context for flattening addition has nested pattern-matching.
--
module FlatF where

import Intro2
import PushNegF as Neg hiding (Ctx)

data Ctx e = CtxNone | CtxAdd (Ctx e -> e)

instance ExpSYM repr => ExpSYM (Ctx repr -> repr) where
  lit n = \c -> case c of
    CtxNone  -> lit n
    CtxAdd e -> add (lit n) (e CtxNone)
  neg e = \c -> case c of
    CtxNone   -> neg (e CtxNone)
    CtxAdd e' -> add (neg (e CtxNone)) (e' CtxNone)
  add e1 e2 = \c -> case c of
    CtxNone   -> e1 (CtxAdd e2)
    CtxAdd e' -> e1 (CtxAdd (add e2 e'))

flata :: (Ctx e -> e) -> e
flata e = e CtxNone

norm = flata . Neg.push_neg

tf3 = (add tf1 (neg (neg tf1)))