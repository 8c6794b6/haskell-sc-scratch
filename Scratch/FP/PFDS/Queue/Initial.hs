{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains initial implementation of Queue shown in chapter 3.

-}
module Queue.Initial where

import Control.Exception
import Data.Typeable
import Prelude hiding (head, tail)
import qualified Prelude

------------------------------------------------------------------------------
-- * Queue
-- 
-- Invariants: qf is empty only if qr is empty.

data Queue a = Queue { qf :: [a], qr :: [a] } deriving Show

q1 :: Queue Int
q1 = Queue [1,2,3] [6,5,4]

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty q = case q of
  Queue [] [] -> True
  _           -> False
  
queue :: Queue a -> Queue a  
queue q = case q of
  Queue [] rs -> Queue (reverse rs) []
  _           -> q
  
------------------------------------------------------------------------------
-- * Exception thrown when operating on empty queue.

data QueueException = EmptyQueueException deriving (Show, Typeable)
instance Exception QueueException

emptyQueueException :: a
emptyQueueException = throw EmptyQueueException

------------------------------------------------------------------------------
-- * Queue operations

head :: Queue a -> a
head q = case q of
  Queue []    _ -> throw EmptyQueueException
  Queue (x:_) _ -> x
  
tail :: Queue a -> Queue a
tail q = case q of
  Queue [] _      -> throw EmptyQueueException
  Queue (_:xs) rs -> queue $ Queue xs rs

snoc :: a -> Queue a -> Queue a
snoc a (Queue xs ys) = queue $ Queue xs (a:ys)
