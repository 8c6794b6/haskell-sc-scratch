------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Tree data type.
--

module LearnYour.Zipper.Tree where

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

freeTree :: Tree Char
freeTree =
  Node 'P'
    (Node 'O'
       (Node 'L'
          (Node 'N' Empty Empty)
          (Node 'T' Empty Empty))
       (Node 'Y'
          (Node 'S' Empty Empty)
          (Node 'A' Empty Empty)))
    (Node 'L'
       (Node 'W'
          (Node 'C' Empty Empty)
          (Node 'R' Empty Empty))
       (Node 'A'
          (Node 'A' Empty Empty)
          (Node 'C' Empty Empty)))


(-:) :: a -> (a -> b) -> b
x -: f = f x
