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
-- Lecture: <http://okmij.org/ftp/tagless-final/course/PushNegFExt.hs>
--
module PushNegFExt where

import Intro2
import PushNegF
import ExtF

instance MulSYM repr => MulSYM (Ctx -> repr) where
  mul e1 e2 = \c -> case c of
    Pos -> mul (e1 Pos) (e2 Pos)
    Neg -> mul (e1 Neg) (e2 Pos)
