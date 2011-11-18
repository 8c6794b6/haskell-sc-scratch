{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

 * http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial

-}
module VectorTut where

import Control.Monad
import Foreign
import Foreign.C.Types

import Control.DeepSeq (NFData(..))
import Criterion.Main

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S

data Vec4 = Vec4
   {-# UNPACK #-} !Float
   {-# UNPACK #-} !Float
   {-# UNPACK #-} !Float
   {-# UNPACK #-} !Float

instance NFData Vec4 where
  rnf (Vec4 a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` ()

a = Vec4 0.2 0.1 0.6 1.0
m = Vec4 0.99 0.7 0.8 0.6

add :: Vec4 -> Vec4 -> Vec4
add (Vec4 a b c d) (Vec4 a' b' c' d') = Vec4 (a+a') (b+b') (c+c') (d+d')
{-# INLINE add #-}

mult :: Vec4 -> Vec4 -> Vec4
mult (Vec4 a b c d) (Vec4 a' b' c' d') = Vec4 (a*a') (b*b') (c*c') (d*d')
{-# INLINE mult #-}

vsum :: Vec4 -> Float
vsum (Vec4 a b c d) = a+b+c+d
{-# INLINE vsum #-}

------------------------------------------------------------------------------
--
-- Using Storable
--

instance Storable Vec4 where
  sizeOf _ = sizeOf (undefined :: Float) * 4
  alignment _ = alignment (undefined :: Float)

  {-# INLINE peek #-}
  peek p = do
    let q = castPtr p
    a <- peekElemOff q 0
    b <- peekElemOff q 1
    c <- peekElemOff q 2
    d <- peekElemOff q 3
    return $ Vec4 a b c d

  {-# INLINE poke #-}
  poke p (Vec4 a b c d) = do
    let q = castPtr p
    pokeElemOff q 0 a
    pokeElemOff q 1 b
    pokeElemOff q 2 c
    pokeElemOff q 3 d

multList :: Int -> S.Vector Vec4 -> S.Vector Vec4
multList !count !src
  | count <= 0 = src
  | otherwise  = multList (count-1) $ S.map (\v -> add (mult v m) a) src

multList_gen :: Int -> V.Vector Vec4 -> V.Vector Vec4
multList_gen !count !src
  | count <= 0 = src
  | otherwise  = multList_gen (count-1) $ V.map (\v -> add (mult v m) a) src

repCount :: Int
repCount = 10000

arraySize :: Int
arraySize = 20000

storable_sum :: IO Float
storable_sum =
  return $ S.sum
    $ S.map vsum
    $ multList repCount
    $ S.replicate arraySize (Vec4 0 0 0 0)

------------------------------------------------------------------------------
--
-- Using Data.Vector
--
-- Compared to S.Vector, V.Vector is using a lot of memory.
-- V.Vector outperforms S.Vector when compiled with profiling options.
--

general_sum :: IO Float
general_sum =
  return $ V.sum
    $ V.map vsum
    $ multList_gen repCount
    $ V.replicate arraySize (Vec4 0 0 0 0)

------------------------------------------------------------------------------
--
-- Using Data.Vector.Unboxed
--

newtype instance U.MVector s Vec4  = MV_Vec4 (U.MVector s (Float,Float,Float,Float))
newtype instance U.Vector Vec4 = V_Vec4 (U.Vector (Float,Float,Float,Float))

instance GM.MVector U.MVector Vec4 where
  {-# INLINE basicLength #-}
  basicLength (MV_Vec4 v) = GM.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (MV_Vec4 v) = MV_Vec4 $ GM.basicUnsafeSlice i n v
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Vec4 v1) (MV_Vec4 v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = MV_Vec4 `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Vec4 v) i = q2v4 `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Vec4 v) i x = GM.basicUnsafeWrite v i (v42q x)

instance G.Vector U.Vector Vec4 where
  {-# INLINE basicLength #-}
  basicLength (V_Vec4 v) = G.basicLength v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Vec4 v) = V_Vec4 `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Vec4 v) = MV_Vec4 `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (V_Vec4 v) = V_Vec4 $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Vec4 v) i = q2v4 `liftM` G.basicUnsafeIndexM v i

instance U.Unbox Vec4


q2v4 :: (Float,Float,Float,Float) -> Vec4
q2v4 (a,b,c,d) = Vec4 a b c d

v42q :: Vec4 -> (Float,Float,Float,Float)
v42q (Vec4 a b c d) = (a,b,c,d)

unboxed_sum :: IO Float
unboxed_sum =
  return $ U.sum
    $ U.map vsum
    $ multList_unboxed repCount
    $ U.replicate arraySize (Vec4 0 0 0 0)

multList_unboxed :: Int -> U.Vector Vec4 -> U.Vector Vec4
multList_unboxed !count !src
  | count <= 0 = src
  | otherwise  = multList_unboxed (count-1) $ U.map (\v -> add (mult v m) a) src

test_vector_difference :: IO Bool
test_vector_difference = do
  s <- storable_sum
  -- g <- general_sum
  u <- unboxed_sum
  -- return $ s == g && g == u
  return $ s == u

main :: IO ()
main = do
  same_answer <- test_vector_difference
  unless same_answer $
    error "storable/unboxed returning differenct result!"
  defaultMain
    [ bench "storable" (nfIO storable_sum)
    -- Using too much memory.
    -- , bench "general" (nfIO general_sum)
    , bench "unboxed" (nfIO unboxed_sum) ]
