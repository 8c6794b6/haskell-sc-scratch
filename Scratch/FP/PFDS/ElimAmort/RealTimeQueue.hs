{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

Realtime Queue, from chapter 4.
-}
module ElimAmort.RealtimeQueue where

import Prelude hiding (head, tail)

import Queue.Initial (emptyQueueException)

data Queue a =
  Queue {qf :: [a], qr :: [a], qs :: [a]} deriving (Show)

empty :: Queue a
empty = Queue [] [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue f _ _) = null f

rotate :: [a] -> [a] -> [a] -> [a]
rotate xs ys zs = case (xs, ys) of
  ([], b:_) -> b : zs
  (a:as, b:bs) -> a : rotate as bs (b:zs)

queue :: [a] -> [a] -> [a] -> Queue a
queue as bs cs = case (as,bs,cs) of
  (f, r, (z:zs)) -> Queue f r zs
  (f, r, [])     ->
    let f' = rotate f r []
    in  Queue f' [] f'

snoc :: Queue a -> a -> Queue a
snoc (Queue f r s) x = queue f (x:r) s

head :: Queue a -> a
head q = case q of
  Queue [] _ _    -> emptyQueueException
  Queue (x:_) _ _ -> x
  
tail :: Queue a -> Queue a  
tail q = case q of  
  Queue [] _ _ -> emptyQueueException
  Queue (x:xs) r s -> queue xs r s
