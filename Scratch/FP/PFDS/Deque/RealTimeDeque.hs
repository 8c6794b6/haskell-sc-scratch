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

This codes contains real time deque, from figure 5.3.

-}
module Deque.RealTimeDeque where

import Prelude hiding (head, tail, init, last)

import Queue.Initial (emptyQueueException)

data RTDeque a = RTDeque
  { c      :: Int -- ^ Threashold constant
  , rtf    :: [a] -- ^ Front list
  , rtflen :: Int -- ^ Length of front list
  , rtsf   :: [a] -- ^ Front stream
  , rtr    :: [a] -- ^ Rear list
  , rtrlen :: Int -- ^ Length of rear list
  , rtsr   :: [a] -- ^ Rear stream
  } deriving Show

empty :: Int -> RTDeque a
empty n = RTDeque n [] 0 [] [] 0 []

isEmpty :: RTDeque a -> Bool
isEmpty (RTDeque _ _ lenf _ _ lenr _)
  | lenf + lenr == 0 = True
  | otherwise        = False

exec1 :: [a] -> [a]
exec1 s = case s of (x:xs) -> xs; _ -> s

exec2 :: [a] -> [a]
exec2 = exec1 . exec1

rotateRev :: Int -> [a] -> [a] -> [a] -> [a]
rotateRev th xs ys zs = case xs of
  []     -> reverse ys ++ zs
  (a:as) -> a : rotateRev th as (drop th ys) (reverse (take th ys) ++ zs)

rotateDrop :: Int -> [a] -> Int -> [a] -> [a]
rotateDrop h xs i zs
  | i < h     = rotateRev h xs (drop i zs) []
  | otherwise =
    let (x':xs') = xs
    in  x' : rotateDrop h xs' (i-h) (drop h zs)

queue :: RTDeque a -> RTDeque a
queue q@(RTDeque th f lenf sf r lenr sr)
  | lenf > th * lenr + 1 =
    let i = (lenf+lenr) `div` 2
        j = lenf + lenr - i
        f' = take i f
        r' = rotateDrop th r i f
    in  RTDeque th f' i f' r' j r'
  | lenr > th * lenf + 1 =
    let i = (lenf+lenr) `div` 2
        j = lenf + lenr - i
        f' = rotateDrop th f j r
        r' = take j r
    in  RTDeque th f' i f' r' j r'
  | otherwise = q

cons :: a -> RTDeque a -> RTDeque a
cons x (RTDeque th f lenf sf r lenr sr) =
  queue $ RTDeque th (x:f) (lenf+1) (exec1 sf) r lenr (exec1 sr)

head :: RTDeque a -> a
head (RTDeque _ f _ _ r _ _) = case (f,r) of
  ([],[])  -> emptyQueueException
  ([],x:_) -> x
  (x:_,_)  -> x

tail :: RTDeque a -> RTDeque a
tail (RTDeque th f lenf sf r lenr sr) = case (f,r) of
  ([],[])   -> emptyQueueException
  ([],x:[]) -> empty th
  (_:xs,_)  -> queue $ RTDeque th xs (lenf-1) (exec2 sf) r lenr (exec2 sr)

snoc :: a -> RTDeque a -> RTDeque a
snoc x (RTDeque th f lenf sf r lenr sr) =
  queue $ RTDeque th f lenf (exec1 sf) (x:r) lenr (exec1 sr)

last :: RTDeque a -> a
last (RTDeque _ f _ _ r _ _) = case (f,r) of
  ([],[])  -> emptyQueueException
  ([],[x]) -> x
  ([x],[]) -> x

init :: RTDeque a -> RTDeque a
init (RTDeque th f lenf sf r lenr sr) = case (f,r) of
  ([],[])  -> emptyQueueException
  ([],[x]) -> empty th
  (_,_:xs) -> RTDeque th f lenf (exec2 sf) xs (lenr-1) (exec2 sr)

toList :: RTDeque a -> [a]
toList (RTDeque _ f _ _ r _ _) = f ++ reverse r
