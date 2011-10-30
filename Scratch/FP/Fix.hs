{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Playing with fix.

-}
module Fix where

import Control.Monad.State hiding (fix)
import qualified Data.Map as M

fix :: (a -> a) -> a
fix f = f (fix f)

fact'01 :: Num a => a -> a
fact'01 n
  | n == 1    = n
  | otherwise = n * fact'01 (n-1)

fact'02 :: Num a => (a -> a) -> a -> a
fact'02 rec n
  | n == 1    = n
  | otherwise =  n * rec (n-1)

fact'02' :: Num a => a -> a
fact'02' = fix fact'02


{-

From:
  http://www.kennknowles.com/blog/2008/05/07/ctl-model-checking-in-haskelle-a-classic-algorithm-explained-as-memoization/
-}

type Gen a = a -> a

fib :: Int -> Int
fib n = case n of
  0 -> 0
  1 -> 1
  _ -> fib (n-1) + fib (n-2)

gfib :: Gen (Int -> Int)
gfib rec n = case n of
  0 -> 0
  1 -> 1
  _ -> rec (n-1) + rec (n-2)

gfib' :: Gen Int
gfib' = fix gfib

-- memoize.

-- mfib :: Monad m => Gen (Int -> m Int)
mfib rec n = case n of
  0 -> return 0
  1 -> return 1
  _ -> do
    a <- rec (n-1)
    b <- rec (n-2)
    return $ a + b

mfib' :: Gen Integer
mfib' n = evalState (fix (mfib . memoize) n) M.empty

type Memoized a b = State (M.Map a b)

memoize :: Ord a => Gen (a -> Memoized a b b)
memoize rec x = do
  memo <- check x
  case memo of
    Just y  -> return y
    Nothing -> do
      y <- rec x
      store x y
      return y

check :: Ord a => a -> Memoized a b (Maybe b)
check a = do
  memotable <- get
  return $ M.lookup a memotable

store :: Ord a => a -> b -> Memoized a b ()
store k v = modify (M.insert k v)

{-
From: http://blog.plover.com/prog/springschool95-2.html
-}

newtype Mu f = In {out :: (f (Mu f))}

instance Show (a (Mu a)) => Show (Mu a) where
  show (In x) = "In (" ++ show x ++ ")"

mm1 = In Nothing
mm2 = In (Just mm1)
mm3 = In (Just mm2)
mm4 = In (Just mm3)

newtype Y a = Y {unY :: Y a -> a}

yfix f = let y x = f (unY x x)
         in  y (Y y)

y = \f -> (\x -> f (unY x x)) (Y (\x -> f (unY x x)))

data Number = Zero | Succ Number deriving (Eq, Show)

data List' a b = Cons a b | Nil deriving (Eq, Show)
type List a = Mu (List' a)

ld1 = Cons 1 (Cons 2 (Cons 3 Nil))

nil :: List a
nil = In Nil

cons :: a -> List a -> List a
cons x xs = In (Cons x xs)

type Elist a = Mu (Either a)

left a = In (Left a)
right a = In (Right a)

emu1 = left ()
emu2 = right emu1
emu3 = right (right (left (right (left (right emu2)))))

-- *Fix> emu1
-- In (Left ())
-- *Fix> emu2
-- In (Right In (Left ()))
-- *Fix> emu3
-- In (Right In (Right In (Left In (Right In (Left In (Right In (Right In (Left ()))))))))
--

data Tree' a b
  = Node a b b
  | Leaf
  deriving (Eq, Show)

type Tree a = Mu (Tree' a)

node a l r = In (Node a l r)
leaf = In Leaf

-- *Fix> node 3 leaf leaf
-- In (Node 3 In (Leaf) In (Leaf))
