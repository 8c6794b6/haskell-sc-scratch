{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* <http://www.haskell.org/haskellwiki/GADTs_for_dummies>

-}
module ForDummies where

import Data.Array.Unboxed (UArray)
import Data.Map (Map)
import Data.Set (Set)

class IsSimple a
instance IsSimple Bool
instance IsSimple Int

class Collection a c
instance Collection a [a]
instance Collection a (Set a)
instance Collection a (Map k a)
instance (Collection a c) => Collection a [c]
instance (IsSimple a) => Collection a (UArray i a)

class HasInt a
instance HasInt Int
instance (HasInt a) => HasInt [a]
instance (HasInt a) => HasInt (Map a b)
-- instance (HasInt b) => HasInt (Map a b)
