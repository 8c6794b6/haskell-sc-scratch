{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Inspired from:
--
-- * <http://efreedom.com/Question/1-3208258/Memoization-Haskell>
--
module Fib where

import Control.Applicative
import qualified Data.Map as M
import "mtl" Control.Monad.State.Lazy hiding (fix)

fix :: (a -> a) -> a
fix f = f (fix f)

f :: (Int -> Int) -> Int -> Int
f mf 0 = 0
f mf n = max n $ mf (div n 2) + mf (div n 3) + mf (div n 4)

f_list :: [Int]
f_list = map (f faster_f) [0..]

faster_f :: Int -> Int
faster_f n = f_list !! n

data Tree a = Tree (Tree a) !a (Tree a)

instance Functor Tree where
  fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n-1) `divMod` 2 of
  (q,0) -> index l q
  (q,1) -> index r q

nats :: Tree Int
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
      where
        l = n + s
        r = l + s
        s' = s * 2

toList :: Tree a -> [a]
toList as = map (index as) [0..]

f_tree :: Tree Int
f_tree = fmap (f fastest_f) nats

fastest_f :: Int -> Int
fastest_f = index f_tree

memoized_fib = (map f [0..] !!)
  where
    f 0 = 0
    f 1 = 1
    f n = memoized_fib (n-2) + memoized_fib (n-1)

memoized_tree_fib = index (fmap f nats)
  where
    f 0 = 0
    f 1 = 1
    f n = memoized_tree_fib (n-2) + memoized_tree_fib (n-1)

fib :: (Int -> Int) -> Int -> Int
fib self 0 = 0
fib self 1 = 1
fib self n = fib self (n-2) + fib self (n-1)

fibM :: (Num a, Num b, Monad m) => (a -> m b) -> a -> m b
fibM f 0 = return 0
fibM f 1 = return 1
fibM f n = f (n-1) >>= \x -> f (n-2) >>= \y -> return (x+y)

type StateMap a b = State (M.Map a b) b

memoizeM :: (Ord k) => ((k -> StateMap k a) -> (k -> StateMap k a)) -> k -> a
memoizeM t x = evalState (f x) M.empty where
  f x = get >>= \m -> maybe (g x) return (M.lookup x m)
  g x = do
    y <- t f x
    m <- get
    put $ M.insert x y m
    return y

memoized_monad_fib = memoizeM fibM

fibA :: (Num a, Num b, Applicative f) => (a -> f b) -> a -> f b
fibA f 0 = pure 0
fibA f 1 = pure 1
fibA f n = (+) <$> f (n-2) <*> f (n-1)

memoized_applicative_fib = memoizeM fibA

fixs :: [[a->b]->a->b] -> [a->b]
fixs fs = self_apply (\pp -> map ($ pp) fs)

self_apply f = f g where g = f g
-- self_apply = fix

test1 = (map iseven [0..5], map isodd [0..5])
  where
    [iseven, isodd] = fixs [fe,fo]
    fe [e,o] x = x == 0 || o (x-1)
    fo [e,o] x = x /= 0 && e (x-1)
    
test2 = map (\f -> map f [0..11]) fs    
  where
    fs = fixs [\[t1,t2,t3] x -> x == 0 || t2 (x-1)
              ,\[t1,t2,t3] x -> (x/=0)&&(x==1) || t1 (x-2)
              ,\[t1,t2,t3] x -> (x==2)||((x>2) && t1 (x-1))]