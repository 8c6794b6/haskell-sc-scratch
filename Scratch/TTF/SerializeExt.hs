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
-- Lecture: <http://okmij.org/ftp/tagless-final/course/SerializeExt.hs>
--
module SerializeExt where

import Intro2
import ExtF 
import Serialize (Tree(..))
import qualified Serialize as S

instance MulSYM Tree where
  mul e1 e2 = Node "Mul" [e1,e2]
  
tfm1_tree = S.toTree tfm1 
tfm2_tree = S.toTree tfm2 

fromTreeExt self t = case t of  
  Node "Mul" [e1,e2] -> mul (self e1) (self e2)
  _                  -> S.fromTreeExt self t
  
fromTree = S.fix fromTreeExt