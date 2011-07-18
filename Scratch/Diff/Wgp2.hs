{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Scratch written while reading gdiff-wgp.pdf.
--
-- Naive implementation of diffing and patching trees.
-- Again, no performance optimizations, straight forward implementation.
-- 
-- ... How to write a spelling corrector
--
-- * <http://norvig.com/spell-correct.html>
-- 
module Wgp2 where

import Wgp1 ((.?))
import Data.Tree

import Sound.SC3.Lepton
import GDiffScratch2
import Sample

-- data Tree label where
--   Node :: label -> [Tree label] -> Tree label
  
-- instance Show label => Show (Tree label) where  
--   show (Node x ns) = "Node " ++ show x ++ " " ++ show ns
  
data Diff label where  
  Ins :: (label,Int) -> Diff label -> Diff label
  Del :: (label,Int) -> Diff label -> Diff label
  Cpy :: (label,Int) -> Diff label -> Diff label
  End :: Diff label
  
instance Show label => Show (Diff label) where
  show (Ins l d) = "Ins " ++ show l ++ ", " ++ show d
  show (Del l d) = "Del " ++ show l ++ ", " ++ show d
  show (Cpy l d) = "Cpy " ++ show l ++ ", " ++ show d
  show End       = "End"

patch :: Eq label => Diff label -> [Tree label] -> Maybe [Tree label]  
patch (Ins x d) ys = (insert x .? patch d) ys
patch (Del x d) ys = (            patch d .? delete x) ys
patch (Cpy x d) ys = (insert x .? patch d .? delete x) ys
patch End []       = Just []
patch End _        = Nothing

insert :: (label,Int) -> [Tree label] -> Maybe [Tree label]
insert (x,n) (splitAt n -> (ys,yss')) = 
  if length ys == n then 
    Just (Node x ys:yss')
  else
    Nothing

delete :: Eq label => (label,Int) -> [Tree label] -> Maybe [Tree label]
delete (x,n) []              = Nothing
delete (x,n) (Node y ys:yss) = 
  if x == y || n == length ys then Just (ys++yss) else Nothing
     
diff :: Eq label => [Tree label] -> [Tree label] -> Diff label
diff [] []                           = End
diff [] (Node y ys:yss)              = Ins (y,length ys) (diff [] (ys++yss))
diff (Node x xs:xss) []              = Del (x,length xs) (diff (xs++xss) [])
diff (Node x xs:xss) (Node y ys:yss) = 
  if (x == y) || (length xs == length ys) then best3 else best2 where
    best2 = Del (x,length xs) (diff (xs++xss) (Node y ys:yss)) /\
            Ins (y,length ys) (diff (Node x xs:xss) (ys++yss))
    best3 = Cpy (x,length xs) (diff (xs++xss) (ys++yss)) /\
            best2

(/\) :: Diff item -> Diff item -> Diff item
dx /\ dy = if cost dx <= cost dy then dx else dy

cost :: Diff a -> Int
cost (Ins _ d) = 1 + cost d
cost (Del _ d) = 1 + cost d
cost (Cpy _ d) = 1 + cost d
cost End       = 0

-- t1 :: Tree Char
-- t1 = Node 'a' 
--        [Node 'b' []
--        ,Node 'c' 
--           [Node 'd' []
--           ,Node 'e' []]]

-- t2 :: Tree Char
-- t2 = Node 'a' 
--        [Node 'b' 
--           [Node 'c' []]
--        ,Node 'd' 
--           [Node 'e' []]]
       
-- t3 :: Tree Char
-- t3 = Node 'e' 
--        [Node 'd' []
--        ,Node 'b' 
--           [Node 'a' []
--           ,Node 'c' []]]
  
main :: IO ()
main = do
  let d1_2 = diff [toRose t1] [toRose t2]
  --     d1_3 = diff [toRose t1] [toRose t3]
  print d1_2
  print $ patch d1_2 [toRose t1]
  -- print d1_3
  -- print $ patch d1_3 [toRose t1]