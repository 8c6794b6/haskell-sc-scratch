{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Scratch writen while reading IterateeIO-talk.pdf.

-}
module Talk where

import Prelude hiding (head)

import Data.IntMap (IntMap, fold)

import qualified Data.IntMap as IM

prod :: Num a => IntMap a -> a
prod = fold (*) 1

coll :: IntMap Int
coll = IM.fromList $ zip [1..] [1..10]

prodbut :: (Ord a, Num a) => a -> Int
prodbut n = snd (fold iteratee (n,1) coll)
  where iteratee a (n,s) =
          if n <= 0 then (n,a*s) else (n-1,s)

data Stream = EOF (Maybe ErrMsg) | Chunk String

type ErrMsg = String

data Iteratee a
  = IE_done a
  | IE_cont (Maybe ErrMsg) (Stream -> (Iteratee a, Stream))
    
peek :: Iteratee (Maybe Char)    
peek = IE_cont Nothing step 
  where
    step s@(Chunk [])    = (peek,s)
    step s@(Chunk (c:_)) = (IE_done (Just c), s)
    step s               = (IE_done Nothing, s)
                          
head :: Iteratee Char                           
head = IE_cont Nothing step
  where
    step (Chunk [])    = (head, Chunk [])
    step (Chunk (c:t)) = (IE_done c, Chunk t)
    step s             = (IE_cont (Just "EOF") step, s)
                        