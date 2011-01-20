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
-- Lecture: <http://okmij.org/ftp/tagless-final/course/Serialize.hs>
--
module Serialize where

import Intro2

data Tree = Leaf String
          | Node String [Tree]
            deriving (Eq, Read, Show)
                     
instance ExpSYM Tree where                     
  lit n = Node "Lit" [Leaf $ show n]
  neg e = Node "Neg" [e]
  add e1 e2 = Node "Add" [e1,e2]
  
toTree :: Tree -> Tree
toTree = id

tf1_tree = toTree tf1

fromTreeExt :: (ExpSYM b) => (Tree -> b) -> Tree -> b
fromTreeExt self t = case t of
  Node "Lit" [Leaf n] -> lit $ read n
  Node "Neg" [e]      -> neg (self e)
  Node "Add" [e1,e2]  -> add (self e1) (self e2)
  _                   -> error $ "Invalid tree: " ++ show t
    
fix :: (t -> t) -> t
fix f = f (fix f)

fromTree :: (ExpSYM b) => Tree -> b
fromTree = fix fromTreeExt