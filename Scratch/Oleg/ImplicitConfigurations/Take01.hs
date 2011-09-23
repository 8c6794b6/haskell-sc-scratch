{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading /Functional Pearl: Implicit Configurations/.

-}
module Take01 where

import Data.Bits
import Foreign.C (CChar)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.StablePtr
  (StablePtr, freeStablePtr, deRefStablePtr, newStablePtr)
import System.IO.Unsafe (unsafePerformIO)

newtype Modulus s a = Modulus a deriving (Eq, Show)
newtype M s a = M a deriving (Eq, Show)

add :: Integral a => Modulus s a -> M s a -> M s a -> M s a
add (Modulus m) (M a) (M b) = M (mod (a+b) m)

mul :: Integral a => Modulus s a -> M s a -> M s a -> M s a
mul (Modulus m) (M a) (M b) = M (mod (a*b) m)

unM :: M s a -> a
unM (M a) = a

test1 m a b = add m (mul m a a) (mul m b b)

withModulus1 :: a -> (forall s. Modulus s a -> w) -> w
withModulus1 m k = k (Modulus m)

test2 = withModulus1 4 $ \m ->
  let a = M 3; b = M 5
  in  unM $ add m (mul m a a) (mul m b b)

class Modular s a | s -> a where
  modulus :: s -> a

normalize :: forall s a. (Modular s a, Integral a) => a -> M s a
normalize a = M (mod (a :: a) (modulus (__ :: s)))

instance (Modular s a, Integral a) => Num (M s a) where
  M a + M b = normalize (a + b)
  M a - M b = normalize (a - b)
  M a * M b = normalize (a * b)
  negate (M a) = normalize (negate a)
  fromInteger i = normalize (fromInteger i)
  signum = error "Modular numbers are not signed"
  abs = error "Modular numbers are not signed"

data Zero
data Twice s
data Succ s
data Pred s

__ = undefined

class ReflectNum s where
  reflectNum :: Num a => s -> a

instance ReflectNum Zero where
  reflectNum _ = 0

instance ReflectNum s => ReflectNum (Twice s) where
  reflectNum _ = reflectNum (__ :: s) * 2

instance ReflectNum s => ReflectNum (Succ s) where
  reflectNum _ = reflectNum (__ :: s) + 1

instance ReflectNum s => ReflectNum (Pred s) where
  reflectNum _ = reflectNum (__ :: s) - 1

reifyIntegral :: Integral a => a -> (forall s. ReflectNum s => s -> w) -> w
reifyIntegral i k = case quotRem i 2 of
  (0, 0) -> k (__ :: Zero)
  (j, 0) -> reifyIntegral j (\(_::s) -> k (__ :: Twice s))
  (j, 1) -> reifyIntegral j (\(_::s) -> k (__ :: Succ (Twice s)))
  (j,-1) -> reifyIntegral j (\(_::s) -> k (__ :: Pred (Twice s)))

data ModulusNum s a

instance (ReflectNum s, Num a) => Modular (ModulusNum s a) a where
  modulus _ = reflectNum (__::s)

withIntegralModulus :: Integral a => a -> (forall s. Modular s a => s -> w) -> w
withIntegralModulus i k = reifyIntegral i (\(_::s) -> k (__ :: ModulusNum s a))

-- This works in ghci:
--
-- > withIntegralModulus (-42::Int) modulus
-- -42
--

test'3 :: (Modular s a, Integral a) => s -> M s a
test'3 _ = 3 * 3 + 5 * 5

-- This works:
-- > withIntegralModulus 4 (unM . test'3)
-- 2

data Nil
data Cons s ss

class ReflectNums ss where
  reflectNums :: Num a => ss -> [a]

instance ReflectNums Nil where
  reflectNums _ = []

instance (ReflectNum s, ReflectNums ss) => ReflectNums (Cons s ss) where
  reflectNums _ = reflectNum (__ :: s) : reflectNums (__ :: ss)

reifyIntegrals :: Integral a => [a] -> (forall ss. ReflectNums ss => ss -> w) -> w
reifyIntegrals [] k = k (__ :: Nil)
reifyIntegrals (i:ii) k =
  reifyIntegral i $ \(_::s) ->
  reifyIntegrals ii $ \(_::ss) ->
  k (__ :: Cons s ss)

-- This works:
--
-- > reifyIntegrals [-10..10] reflectNums
-- [-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
--

type Byte = CChar

data Store s a

class ReflectStorable s where
  reflectStorable :: Storable a => s a -> a

instance ReflectNums s => ReflectStorable (Store s) where
  reflectStorable _ = unsafePerformIO $ alloca $ \p ->
    pokeArray (castPtr p) bytes >> peek p
    where
      bytes = reflectNums (__ :: s) :: [Byte]

reifyStorable ::
  Storable a
  => a -> (forall s. ReflectStorable s => s a -> w) -> w
reifyStorable a k =
  reifyIntegrals (bytes :: [Byte]) (\(_ :: s) -> k (__ :: Store s a))
  where
    bytes = unsafePerformIO $ with a (peekArray (sizeOf a) . castPtr)

-- Works:
--
-- > reifyStorable (2.5 :: Double) reflectStorable
-- 2.5

data Stable (s :: * -> *) a

class Reflect' s a | s -> a where
  reflect' :: s -> a

instance ReflectStorable s => Reflect' (Stable s a) a where
  reflect' = unsafePerformIO $ do
    a <- deRefStablePtr p
    return (const a)
    where p = reflectStorable (__ :: s p)

reify' :: a -> (forall s. Reflect' s a => s -> w) -> w
reify' (a :: a) k = unsafePerformIO $ do
  p <- newStablePtr a :: IO (StablePtr a)
  reifyStorable p (\(_::s (StablePtr a)) -> k' (__ :: Stable s a))
  where
    k' s = return (k s)

-- Works:
--
-- > reify (Just 2.5 :: Maybe Double) reflect
-- Just 2.5
--

class Reflect s a | s -> a where
  reflect :: s -> a
  
instance ReflectStorable s => Reflect (Stable s a) a where
  reflect = unsafePerformIO $ do
    a <- deRefStablePtr p
    freeStablePtr p
    return (const a)
    where
      p = reflectStorable (__ :: s p)

reify :: a -> (forall s. Reflect s a => s -> w) -> w
reify (a::a) k = unsafePerformIO $ do
  p <- newStablePtr a
  reifyStorable p (\(_::s (StablePtr a)) -> k' (__::Stable s a))
  where
    k' (s :: s) = (reflect :: s -> a) `seq` return (k s)

-- Works:
-- > reify (Just 2.5 :: Maybe Double) reflect
-- Just 2.5
-- > reify (BogusID 

data ModulusAny s

instance Reflect s a => Modular (ModulusAny s) a where
  modulus _ = reflect (__ :: s)

withModulus :: a -> (forall s. ModulusAny s -> w) -> w
withModulus a k = reify a (\(_::s) -> k (__::ModulusAny s))

withIntegralModulus' :: 
  forall a w. Integral a => a -> (forall s. Modular s a => M s w) -> w
withIntegralModulus' (i::a) k = 
  reifyIntegral i (\(_::t) -> unM (k :: M (ModulusNum t a) w))
  
test4' :: (Modular s a, Integral a) => M s a
test4' = 3*3 + 5*5

test4 :: Int
test4 = withIntegralModulus' 4 test4'

-- Works:
--
-- > test4
-- 4

data Even p q u v a = E a a deriving (Eq, Show)

normalizeEven :: forall p q u v a. 
  (ReflectNum p, ReflectNum q, Integral a, Bits a) =>
  a -> a -> Even p q u v a
normalizeEven a b = 
  E (a .&. shiftL 1 (reflectNum (__ :: p)) - 1)
    (mod b (reflectNum (__ :: q)))
  
instance (ReflectNum p, ReflectNum q,  
          ReflectNum u, ReflectNum v,
          Integral a, Bits a) => Num (Even p q u v a) where
  E a1 b1 + E a2 b2 = normalizeEven (a1+a2) (b1+b2)
  E a1 b1 * E a2 b2 = normalizeEven (a1*a2) (b1*b2)
  E a1 b1 - E a2 b2 = normalizeEven (a1-a2) (b1-b2)
  signum _ = error "signum not defined for Even"
  abs _ = error "abs not defined for Even"
  fromInteger n = normalizeEven (fromInteger n) (fromInteger n)
