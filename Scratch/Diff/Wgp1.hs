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
-- Naive implementation of diffing and patching list.
-- No performance optimizations are used, straight forward implementation.
-- 
module Wgp1 where

main = do
  let d1 = diff "flower" "power"
  putStrLn "Result of 'diff \"flower\" \"power\"' is:"
  print d1
  putStrLn "Result of 'patch (diff \"flower\" \"power\") \"flower\") is:"
  print $ patch d1 "flower"

data Diff item where
  Ins :: item -> Diff item -> Diff item
  Del :: item -> Diff item -> Diff item
  Cpy :: item -> Diff item -> Diff item
  End :: Diff item
  
instance Show item => Show (Diff item) where
  show (Ins x d) = "Ins " ++ show x ++ ", " ++ show d
  show (Del x d) = "Del " ++ show x ++ ", " ++ show d
  show (Cpy x d) = "Cpy " ++ show x ++ ", " ++ show d
  show End       = "End"

(.?) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
(g .? f) (f -> Nothing) = Nothing
(g .? f) (f -> Just y)  = g y

patch (Ins x d) ys = (insert x .? patch d) ys
patch (Del x d) ys = (            patch d .? delete x) ys
patch (Cpy x d) ys = (insert x .? patch d .? delete x) ys
patch End []       = Just []
patch End _        = Nothing

insert :: item -> [item] -> Maybe [item]
insert x ys = Just (x:ys)

delete :: Eq item => item -> [item] -> Maybe [item]
delete x []     = Nothing
delete x (y:ys) = if x==y then Just ys else Nothing

cost :: Diff a -> Int
cost (Ins _ d) = 1 + cost d
cost (Del _ d) = 1 + cost d
cost (Cpy _ d) = 1 + cost d
cost End       = 0

diff :: Eq item => [item] -> [item] -> Diff item
diff [] [] = End
diff [] (y:ys) = Ins y (diff [] ys)
diff (x:xs) [] = Del x (diff xs [])
diff (x:xs) (y:ys) = if x == y then best3 else best2 where
  best2 = Del x (diff xs (y:ys)) /\ 
          Ins y (diff (x:xs) ys)
  best3 = Cpy x (diff xs ys) /\ 
          best2
          
(/\) :: Eq item => Diff item -> Diff item -> Diff item
dx /\ dy = if cost dx <= cost dy then dx else dy
