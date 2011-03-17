------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (Rank2Types, FlexibleContexts)
--
-- Expression of pattern DSL.
--
module Sound.SC3.Lepton.Pattern.Expression
  ( -- * Fundamental
    Pval(..), Plist(..),

    -- * Monoid alike
    Pempty(..), Pappend(..), Pconcat(..),

    -- * Looping
    Pseq(..), Pcycle(..), Pforever(..), Prepeat(..), Preplicate(..),

    -- * Random
    Prandom(..), Prange(..), Pchoose(..), Prand(..), Pshuffle(..),

    -- * Lambda and application (experimental)
    Plam(..), Papp(..)
  ) where

import System.Random (Random)

------------------------------------------------------------------------------
--
-- Expressions
--
------------------------------------------------------------------------------

-- | Lifts given value to pattern.
class Pval p where
  pval :: a -> p a

-- | Empty pattern.
class Pempty p where
  pempty :: p a

-- | Make pattern from list.
class Plist p where
  plist :: [a] -> p a

-- | Make pattern from list of patterns.
class Pconcat p where
  pconcat :: [p a] -> p a

class Pappend p where
  pappend :: p a -> p a -> p a

class Pseq p where
  pseq :: p Int -> [p a] -> p a

class Preplicate p where
  preplicate :: p Int -> p a -> p a

class Pcycle p where
  pcycle :: [p a] -> p a

class Prepeat p where
  prepeat :: a -> p a

class Pforever p where
  pforever :: p a -> p a

class Prandom p where
  prandom :: (Random r) => p r

class Prange p where
  prange  :: (Random r) => p r -> p r -> p r

class Pchoose p where
  pchoose :: p Int -> [p a] -> p a

class Prand p where
  prand :: p Int -> [p a] -> p a

class Pshuffle p where
  pshuffle :: [p a] -> p a

class Plam p where
  plam :: (p a -> p b) -> p (a->b)

class Papp p where
  papp :: p (a->b) -> p a -> p b
