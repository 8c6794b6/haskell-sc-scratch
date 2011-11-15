{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains banker's method implementation of Queue shown in chapter 3.

-}
module Queue.Banker where

import Prelude hiding (head, tail)
import qualified Prelude

import Queue.Initial (emptyQueueException)

------------------------------------------------------------------------------
-- * Bankers Queue 
--
-- Invariants: |bqf| >= |bqr|, lenbqf = |bqf|, lenbqr = |bqr|

data BQueue a = BQueue
 { bqf :: [a], lenbqf :: Int
 , bqr :: [a], lenbqr :: Int
 } deriving Show
            
empty :: BQueue a
empty = BQueue [] 0 [] 0

isEmpty :: BQueue a -> Bool
isEmpty q = lenbqf q == 0

queue :: BQueue a -> BQueue a
queue q@(BQueue fs flen rs rlen)
  | rlen <= flen = q
  | otherwise    = BQueue (fs ++ reverse rs) (flen + rlen) [] 0
                   
------------------------------------------------------------------------------                   
-- * Queue operations

snoc :: a -> BQueue a -> BQueue a
snoc a (BQueue fs lenf rs lenr) = queue $ BQueue fs lenf (a:rs) (lenr+1)

head :: BQueue a -> a
head q = case q of
  BQueue [] _ _ _     -> emptyQueueException
  BQueue (x:xs) _ _ _ -> x

tail :: BQueue a -> BQueue a
tail q = case q of
  BQueue [] _ _ _ -> emptyQueueException
  BQueue (x:fs) flen rs rlen -> queue $ BQueue fs (flen-1) rs rlen
  
------------------------------------------------------------------------------  
-- * Sample queue

bq1 :: BQueue Int
bq1 = BQueue [1,2,3] 3 [6,5,4] 3