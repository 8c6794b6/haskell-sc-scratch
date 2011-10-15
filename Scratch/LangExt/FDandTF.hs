{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Comparing Functional Dependencies (FD) and Type Families (TF).

Inspired from:

* <http://www.haskell.org/pipermail/haskell-cafe/2009-February/055890.html>

Exerpt from above link, with rewrite:

   Theoritically, FD and TF has same expressiveness. Though when mixed with
   other language extension, their differences get more distinguishable
   ... at least in current GHC implementation.

-}
module FDandTF where

class F1 a r | a -> r
instance F1 Bool Int

data T1 a = forall b. F1 a b => MkT1 b

-- | This `add` definition:
--
-- > add :: T1 Bool -> T1 Bool -> T1 Bool
-- > add (MkT1 x) (MkT1 y) = MkT1 (x+y)
--
-- Shows error:
--
-- > Could not deduce (b1 ~ b)
-- > from the context (F1 t b)
-- >   bound by a pattern with constructor
-- >              MkT1 :: forall a b. F1 a b => b -> T1 a,
-- >            in an equation for `add'
--
-- So, make another data type wrapper for F1 class.
--
data T11 a = MkT11 {unMKt11 :: forall b. (F1 a b, Num b) => b}

-- | This time, working.
add11 :: T11 Bool -> T11 Bool -> T11 Bool
add11 (MkT11 x) (MkT11 y) = MkT11 (x+y)

-- | Same example, with type familiy.
type family F2 a
type instance F2 Bool = Int

data T2 a = MkT2 (F2 a)

add2 :: T2 Bool -> T2 Bool -> T2 Bool
add2 (MkT2 x) (MkT2 y) = MkT2 (x+y)

-- | From this 'same' definition:
--
-- > same :: F1 Bool a => a -> Int
-- > same = id
--
-- Get this error:
--
-- > Could not deduce (a ~ Int)
-- > from the context (F1 Bool a)
-- >   bound by the type signature for same :: F1 Bool a => a -> Int
--
-- However, below TF version works.
--
same :: F2 Bool -> Int
same = id