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

Bootstrapped queue, showin in Figure 7.1.
-}
module Bootstrap.BSQueue where

import Prelude hiding (head, tail)

import Queue.Initial (emptyQueueException)

data Queue a
  = Empty
  | Queue [a] (Queue [a]) Int [a] Int
  deriving (Show)

empty :: Queue a
empty = Empty

isEmpty :: Queue a -> Bool
isEmpty q = case q of Empty -> True; _ -> False

queue :: Queue a -> Queue a
queue q = case q of
  Queue f m lenfm r lenr
    | lenr <= lenfm -> checkF q
    | otherwise -> checkF $ Queue f (snoc (reverse r) m) (lenfm+lenr) [] 0
  _ -> q

checkF :: Queue a -> Queue a
checkF q = case q of
  Queue [] Empty _ _ _ -> Empty
  Queue [] m lenfm r lenr -> Queue (head m) (tail m) lenfm r lenr
  _ -> q

snoc :: a -> Queue a -> Queue a
snoc x q = case q of
  Empty -> Queue [x] empty 1 [] 0
  Queue f m lenfm r lenr -> queue $ Queue f m lenfm (x:r) (lenr+1)

head :: Queue a -> a
head q = case q of
  Empty               -> emptyQueueException
  Queue (x:_) _ _ _ _ -> x

tail :: Queue a -> Queue a
tail q = case q of
  Empty                       -> emptyQueueException
  Queue (_:xs) m lenfm r lenr -> queue $ Queue xs m (lenfm-1) r lenr
