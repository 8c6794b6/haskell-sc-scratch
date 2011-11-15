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

This codes contains banker's deque, from figure 5.2.

-}
module Deque.BankersDeque where

import Prelude hiding (head, tail, last, init)

import Queue.Initial (emptyQueueException)

-- | Invariants:
--
-- * c > 1
--
-- * |F| <= c |R| + 1, |R| <= c |F| + 1, LenF = |F|, LenR = |R|
--
data BDeque a = BDeque
  { c      :: Int -- ^ Threshold Constant
  , bqf    :: [a] -- ^ Front
  , lenbqf :: Int -- ^ Length of front
  , bqr    :: [a] -- ^ Rear
  , lenbqr :: Int -- ^ Length of rear
  } deriving (Show)

empty :: Int -> BDeque a
empty n = BDeque n [] 0 [] 0

isEmpty :: BDeque a -> Bool
isEmpty (BDeque _ _ lenf _ lenr)
  | lenf + lenr == 0 = True
  | otherwise        = False

queue :: BDeque a -> BDeque a
queue q@(BDeque c f lenf r lenr)
  | lenf > c * lenr + 1 =
    let i = (lenf + lenr) `div` 2
        j = lenf + lenr - i
        (f',r') = splitAt i f
        r'' = r ++ reverse r'
    in  BDeque c f' i r'' j
  | lenr > c * lenf + 1 = 
      let i = (lenf + lenr) `div` 2
          j = lenf + lenr - i
          (f',r') = splitAt i r
          f'' = reverse f' 
      in  BDeque c f'' i r' j
  | otherwise = q

cons :: a -> BDeque a -> BDeque a
cons x (BDeque c f lenf r lenr) = queue $ BDeque c (x:f) (lenf+1) r lenr

head :: BDeque a -> a
head (BDeque _ f _ r _) = case (f,r) of
  ([],[])  -> emptyQueueException
  ([],x:_) -> x
  (x:_,_)  -> x
  
tail :: BDeque a -> BDeque a  
tail (BDeque c f lenf r lenr) = case (f,r) of
  ([],[])  -> emptyQueueException
  ([],x:_) -> empty c
  (x:xs,_) -> BDeque c xs (lenf - 1) r lenr
  
snoc :: a -> BDeque a -> BDeque a  
snoc x (BDeque c f lenr r lenf) = queue $ BDeque c f lenf (x:r) (lenr+1)

last :: BDeque a -> a
last (BDeque _ f _ r _) = case (f,r) of
  ([],[]) -> emptyQueueException
  (_,x:_) -> x

init :: BDeque a -> BDeque a
init (BDeque c f lenf r lenr) = case (f,r) of
  ([],[])  -> emptyQueueException
  (_,_:ys) -> BDeque c f lenf ys (lenr-1)
  
toList :: BDeque a -> [a]
toList (BDeque _ f _ r _) = f ++ reverse r
