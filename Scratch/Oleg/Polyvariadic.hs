{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with polyvariadic functions.
--
module Polyvariadic where

import Data.Monoid

import Sound.SC3.Lepton

intro1 = id id id id id True

intro2 = id (\k -> k 1) (\v k -> k (v,True)) (\v k -> k (v, "str")) id

intro3 = begin (push 1) (push 2) (push 3) mul (push 4) add add id
 where
   go :: a -> (a -> b) -> b
   go = flip ($)

   begin = go ()
   begin :: (() -> a) -> a

   push :: a -> b -> ((a,b) -> c) -> c
   push x s = go (x,s)

   add :: (Int, (Int, a)) -> ((Int, a) -> b) -> b
   add (x,(y,s)) = go (x+y,s)

   mul :: (Int, (Int, a)) -> ((Int, a) -> b) -> b
   mul (x,(y,s)) = go (x*y,s)

-- class Buildlist r | a -> r where
--   build' ::

class IntroC a where
  introc :: String -> a

intro4 = putStrLn $ introc "hello, " True " world" '!'

instance IntroC String where
  introc s = s

instance IntroC x => IntroC (Char -> x) where
  introc a = \c -> introc (a ++ [c])

instance IntroC x => IntroC (Bool -> x) where
  introc a = \b -> introc (a ++ show b)

instance IntroC x => IntroC (String -> x) where
  introc a = \s -> introc (a ++ s)

class IntroD a where
  add :: Int -> a

instance IntroD Int where
  add x = x

instance (IntroD a) => IntroD (Int -> a) where
  add x = \y -> add (x+y)

-- Hm, can we avoid writing those type signatures?:
--
-- > add (1::Int) (2::Int) (3::Int)
--

--
-- Let's do some fun with tree building functions.
--
-- Currently, there's Group and Synth constructor as:
--
-- SCNode = Group Int [SCNode] | Synth Int [SynthParam]
--
-- Hence, we write as:
--
-- g1 = Group 1 [Group 10 [Synth 100 []], Synth 11 []]
--
-- Now, make grp and syn function that enable to write as:
--
-- g1' = grp 1 (grp 10 (syn 100 [])) (syn 11 [])
--

class BuildList a r | r -> a where
  build' :: [a] -> a -> r

instance BuildList a [a] where
  build' l x = reverse $ x:l

instance BuildList a r => BuildList a (a->r) where
  build' l x = \y -> build' (x:l) y

build x = build' [] x

class G g where
  grp' :: [SCNode] -> Int -> SCNode -> g

instance G SCNode where
  grp' ns i n = Group i $ reverse (n:ns)

instance G r => G (SCNode -> r) where
  grp' ns i n = \x -> grp' (n:ns) i x

grp :: (G g) => Int -> SCNode -> g
grp = grp' []

syn i n = Synth i n

n1 :: SCNode
n1 =
  grp 1
    (grp 10
       (syn 1000 "foo" []) (syn 1001 "foo" []))
    (syn 2000 "bar" [])

--
-- Need to put type signature when last node is group.
--

n2 :: SCNode
n2 =
  grp 1
    (syn 1000 "foo" [])
    (grp 10 (syn 1000 "foo1" [])
            (syn 1001 "foo2" []) :: SCNode)

-- n2 :: SCNode
-- n2 =
--   grp 1
--    (grp 10
--      (grp 100
--        (syn 1000 "foo" []) (syn 1001 "foo" []))
--      (grp 100
--        (syn 1000 "foo" []) (syn 1001 "foo" []))
--      (grp 101
--        (syn 1010 "bar" []) (syn 1011 "bar" [])))

-- grp n x = Group n (build x)

class Builder2 c a b r | r -> a where
  build2 :: (a -> b -> c a b -> c a b) -> c a b -> a -> b -> r

instance Builder2 c k v (c k v) where
  build2 acc seed k v = acc k v seed

instance Builder2 c a b r => Builder2 c a b (a->b->r) where
  build2 acc seed a b x y = build2 acc (acc a b seed) x y

newtype AL a b = AL {unAL::[(a,b)]} deriving Show

bb1 :: AL String Bool
bb1 = build2 (\x y (AL l) -> AL $ (x,y):l) (AL []) "a" True "b" False

bb1', bb1'', bb1''' :: AL String Bool
bb1' = build2 bbf (AL []) "a" True "b" False
bb1'' = build2 bbf bbv "a" True "b" False
bb1''' = bb' "a" True "b" False

-- bb' = build2 (\x y (AL l) -> AL $ (x,y):l) (AL [])
bb' :: (Builder2 AL a b r) => a -> b -> r
bb' = build2 bbf bbv
bbf x y (AL l) = AL $ (x,y):l
bbv = AL []
