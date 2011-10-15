{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading typefun.pdf.
From /5 Phantom types/.

-}
module Scratch05 where

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Four  = Succ (Succ Two)
type Six   = Succ (Succ Four)
type Eight = Succ (Succ Six)

class Nat n where
  toInt :: n -> Int

instance Nat Zero where
  toInt _ = 0

-- | Needs 'ScopedTypeVariables' language pragma.
instance Nat n => Nat (Succ n) where
  toInt _ = 1 + toInt (undefined :: n)

newtype Pointer n = MkPointer Int
newtype Offset n = MkOffset Int

multiple :: forall n. (Nat n) => Int -> Offset n
multiple i = MkOffset (i * toInt (undefined :: n))

add :: Pointer m -> Offset n -> Pointer (GCD Zero m n)
add (MkPointer x) (MkOffset y) = MkPointer (x+y)

type family GCD d m n
type instance GCD d Zero Zero = d
type instance GCD d (Succ m) (Succ n) = GCD (Succ d) m n
type instance GCD Zero (Succ m) Zero = Succ m
type instance GCD (Succ d) (Succ m) Zero = GCD (Succ Zero) d m
type instance GCD Zero Zero (Succ n) = Succ n
type instance GCD (Succ d) Zero (Succ n) = GCD (Succ Zero) d n


{-|

Works, pointer with multiple of 4 will pass:

> ghci> fetch32 (MkPointer 0 :: Pointer Eight)
> fetching 32

When not, rejected:

> ghci> fetch32 (MkPointer 0 :: Pointer Six)
> <interactive>:1:1:
>     Couldn't match type `Zero' with `Two'
>     Expected type: Four
>       Actual type: GCD Zero Six Four
>     In the expression: fetch32 (MkPointer 0 :: Pointer Six)
>     In an equation for `it': it = fetch32 (MkPointer 0 :: Pointer Six)

-}
fetch32 :: (GCD Zero n Four ~ Four) => Pointer n -> IO ()
fetch32 _ = putStrLn "fetching 32"

class PMonad m where
  unit :: a -> m p p a
  bind :: m p q a -> (a -> m q r b) -> m q r b

data Nil
data Cons l s

data Locked
data Unlocked

newtype LockM p q a = LockM {unLockM :: IO a}

instance PMonad LockM where
  unit x = LockM (return x)
  bind m k = LockM (unLockM m >>= unLockM . k)

lput :: String -> LockM p p ()
lput = LockM . putStrLn

type family Get n p
type instance Get Zero (Cons e p) = e
type instance Get (Succ n) (Cons e p) = Get n p

type family Set n e' p
type instance Set Zero e' (Cons e p) = Cons e' p
type instance Set (Succ n) e' (Cons e p) = Cons e (Set n e' p)

newtype Lock n = Lock Int deriving Show

mkLock :: forall n. Nat n => Lock n
mkLock = Lock (toInt (undefined :: n))

lock1 :: Lock One
lock1 = mkLock :: Lock One

lock2 :: Lock Two
lock2 = mkLock :: Lock Two

acquire :: (Get n p ~ Unlocked) =>
           Lock n -> LockM p (Set n Locked p) ()
acquire l = LockM (putStrLn ("acquire " ++ show l))

release :: (Get n p ~ Locked) =>
           Lock n -> LockM p (Set n Unlocked p) ()
release l = LockM (putStrLn ("release " ++ show l))

type ThreeLocks = Cons Unlocked (Cons Unlocked (Cons Unlocked Nil))

run :: LockM ThreeLocks ThreeLocks a -> IO a
run = unLockM

with1 a =
  acquire lock1 `bind` \_ ->
  a `bind` \x ->
  release lock1 `bind` \_ ->
  unit x
