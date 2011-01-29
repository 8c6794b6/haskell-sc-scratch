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
-- Lecture: <http://okmij.org/ftp/tagless-final/course/PushNegFI.hs>
--
--
module PushNegFI where

import Intro2
import Intro1 (Exp(..))
import qualified PushNegI as I

instance ExpSYM Exp where
  lit = Lit
  neg = Neg
  add = Add

initialize :: Exp -> Exp
initialize = id

finalize :: ExpSYM repr => Exp -> repr
finalize e = case e of
  Lit n     -> lit n
  Neg e     -> neg (finalize e)
  Add e1 e2 -> add (finalize e1) (finalize e2)

push_neg = finalize . I.push_neg . initialize