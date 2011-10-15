{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fcontext-stack=128 #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (Many LANGUAGE pragmas, context stack increased)

Scratch written while reading: <http://okmij.org/ftp/Haskell/TypeLC.lhs>

-}
module TypeLC where

data HTrue = HTrue
instance Show HTrue where show _ = "HTrue"

data HFalse = HFalse
instance Show HFalse where show _ = "HFalse"

data Zero = Zero
instance Show Zero where show _ = "N0"

data Su a = Su a
instance Nat x => Show (Su x) where
  show x = "N" ++ show (fromNat x)

class Nat a where fromNat :: a -> Integer
instance Nat Zero where fromNat _ = 0
instance Nat x => Nat (Su x) where fromNat _ = succ (fromNat (undefined::x))


-- | Indicator for functions, or applicable things
data F x

{-|
The meaning of |A l a b| is that the application to |a| of an applicable
thing denoted by |l| yields |b|.
-}
class A l a b | l a -> b

-- | Boolean not function
data FNot

instance A (F FNot) HTrue HFalse
instance A (F FNot) HFalse HTrue

-- | Boolean and function
data FAnd

instance A (F FAnd) x (F (FAnd,x))

instance A (F (FAnd,HTrue)) a a
instance A (F (FAnd,HFalse)) a HFalse

-- | Syntax for building sequences of applications
data f :< x
infixl 1 :<

-- | The big-step evaluation function
class E a b | a -> b

instance E HTrue HTrue
instance E HFalse HFalse
instance E Zero Zero

instance E (F x) (F x)

instance (E y y', A (F x) y' r) => E ((F x) :< y) r
instance (E (x :< y) r', E (r' :< z) r) => E ((x :< y) :< z) r

instance E x x' => E (Su x) (Su x')

testb x = (&&) (not x) (not (not x))

type Testb x =
  E (F FAnd :< (F FNot :< x) :< (F FNot :< (F FNot :< x))) r => r

testb1_t = undefined :: Testb HTrue
testb1_f = undefined :: Testb HFalse

-- | Applicative fix point combinator
data Rec l
instance E (l :< (F (Rec l)) :< x) r => A (F (Rec l)) x r

fix f = f (fix f)

------------------------------------------------------------------------------
-- Sum

vsum = fix (\self n m -> case n of
               0 -> m
               n -> 1 + self (n-1) m)

-- | Non resursive sum
data FSum'

instance A (F FSum') self (F (FSum',self))
instance A (F (FSum',self)) n (F (FSum',self,n))
instance A (F (FSum',self,Zero)) m m
instance E (self :< n :< m) r => A (F (FSum',self,(Su n))) m (Su r)

-- | Tying the knot for sum
type FSum = Rec (F FSum')

type N0 = Zero; type N1 = Su N0; type N2 = Su N1; type N3 = Su N2;
(n0::N0, n1::N1, n2::N2, n3::N3) = undefined

test_sum :: E (F FSum :< x :< y) r => x -> y -> r
test_sum = undefined

test_sum_2_3 = test_sum n2 n3

-- ghci> :t test_sum_2_3
-- test_sum_2_3 :: Su (Su (Su (Su (Su Zero))))
--
-- ghci> test_sum_2_3
-- N5

------------------------------------------------------------------------------
-- Fibonacci

vfib = fix (\self n -> case n of
               0 -> 1
               1 -> 1
               n -> self (n-1) + self (n-2))

data Fib'

instance A (F Fib') self (F (Fib',self)) -- build closure
instance A (F (Fib',self)) Zero (Su Zero)
instance A (F (Fib',self)) (Su Zero) (Su Zero)
instance E (F FSum :< (self :< n) :< (self :< Su n)) r
  => A (F (Fib',self)) (Su (Su n)) r

type Fib = Rec (F Fib')

test_fib :: E (F Fib :< n) r => n -> r
test_fib = undefined

test_fib_5 = test_fib (test_sum n3 n2)
test_fib_6 = test_fib (test_sum n3 n3)
test_fib_7 = test_fib (test_sum n1 (test_sum n3 n3))
test_fib_8 = test_fib test_fib_5
test_fib_9 = test_fib (test_sum n3 (test_sum n3 n3))

-- ghci> test_fib (test_sum n3 (test_sum n3 n3))
-- N55

------------------------------------------------------------------------------
-- S and K combinators

-- | K combinator
data CombK
instance A (F CombK) a (F (CombK,a))
instance A (F (CombK,a)) b a

-- | S combinator
data CombS
instance A (F CombS) f (F (CombS,f))
instance A (F (CombS,f)) g (F (CombS,f,g))
instance E (f :< x :< (g :< x)) r => A (F (CombS,f,g)) x r

-- | SKK, as the identity
type Test_skk x = E (F CombS :< F CombK :< F CombK :< x) r => r
test_skk1 = undefined :: Test_skk HTrue

type CombZ = F CombS :< F CombK
type CombSu = F CombS :< (F CombS :< (F CombK :< F CombS) :< F CombK)
type CombTwo = CombSu :< (CombSu :< CombZ)
test_ctwo :: E (CombTwo :< (F FSum :< Su Zero) :< Zero) r => r
test_ctwo = undefined