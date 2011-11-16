{-# LANGUAGE NoImplicitPrelude #-}
{-|
module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

Catenable list, showin in Figure 7.5.
-}
module Bootstrap.CatList where

import Prelude hiding (head, tail, (++))

import Queue.Initial (emptyQueueException)
import qualified Bootstrap.BSQueue as Q

data Cat a = Empty | Cat a (Q.Queue (Cat a)) deriving (Show)

empty :: Cat a
empty = Empty

isEmpty :: Cat a -> Bool
isEmpty c = case c of Empty -> True; _ -> False

link :: Cat a -> Cat a -> Cat a
link (Cat x q) s = Cat x (Q.snoc s q)

linkAll :: Q.Queue (Cat a) -> Cat a
linkAll Q.Empty = Empty
linkAll q = case (Q.head q, Q.tail q) of
  (t, q') | Q.isEmpty q' -> t
          | otherwise    -> link t (linkAll q')

(++) :: Cat a -> Cat a -> Cat a
xs ++ ys = case (xs,ys) of
  (_,Empty) -> xs
  (Empty,_) -> ys
  _         -> link xs ys

cons :: a -> Cat a -> Cat a
cons a xs = Cat a Q.empty ++ xs

snoc :: a -> Cat a -> Cat a
snoc a xs = xs ++ Cat a Q.empty

head :: Cat a -> a
head c = case c of
  Empty   -> emptyQueueException
  Cat x _ -> x

tail :: Cat a -> Cat a
tail q = case q of
  Empty -> emptyQueueException
  Cat x q' | Q.isEmpty q' -> Empty
           | otherwise    -> linkAll q'

------------------------------------------------------------------------------
-- Test

toList :: Cat a -> [a]
toList c = case c of
  Empty   -> []
  Cat a q -> a : concatMap toList (Q.toList q)