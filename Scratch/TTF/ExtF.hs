{-# LANGUAGE TypeSynonymInstances #-}
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
-- Lecture: <http://okmij.org/ftp/tagless-final/course/ExtF.hs>
--
module ExtF where

import Intro2 as F hiding (tfl1)

class MulSYM repr where
  mul :: repr -> repr -> repr
  
instance MulSYM Int where  
  mul e1 e2 = e1 * e2
  
instance MulSYM String where  
  mul e1 e2 = "(" ++ e1 ++ " * " ++ e2 ++ ")"
  
tfm1 = add (lit 7) (neg (mul (lit 1) (lit 2)))
tfm2 = mul (lit 7) tf1

tfl1 = [F.tf1]
tfl2 = tfm1 : tfm2 : tfl1