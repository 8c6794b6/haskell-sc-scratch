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
    Pempty(..), Pval(..), Prepeat(..), Plist(..),

    -- * Monoid alike
    Pappend(..), Pconcat(..),

    -- * Looping
    Pseq(..), Pcycle(..), Pforever(..), Preplicate(..),

    -- * Random
    Prandom(..), Prange(..), Pchoose(..), Prand(..), Pshuffle(..),

    -- * Lambda and application (experimental)
    Plam(..), Papp(..),

    -- * Parallel
    Pmerge(..), Ppar(..), Mergable(..),

    -- * OSC message
    Psnew(..), Pnset(..)

  ) where

import System.Random (Random)

import Sound.SC3

import Sound.SC3.Lepton.Pattern.ToOSC

------------------------------------------------------------------------------
--
-- Expressions
--
------------------------------------------------------------------------------

-- | Empty pattern.
class Pempty p where
  pempty :: p a

-- | Lifts given value to pattern.
class Pval p where
  pval :: a -> p a

class Prepeat p where
  prepeat :: a -> p a

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

class Pmerge p where
  pmerge :: Mergable (p m) => p m -> p m -> p m

class Ppar p where
  ppar :: Mergable (p m) => [p m] -> p m

-- | Class for merging patterns in parallel.
class Mergable m where
  merge :: m -> m -> m

-- | Pattern for 's_new' messages.
class Psnew s where
  psnew :: String
    -- ^ Synthdef name.
    -> Maybe Int
    -- ^ Node id for new synth. 'Nothing' for auto generated id by server.
    -> AddAction
    -- ^ Add action in 's_new' message.
    -> Int
    -- ^ Add target id.
    -> [(String, s Double)]
    -- ^ Parameter name and its values.
    -> s (ToOSC Double)

-- | Pattern for 'n_set' message.
class Pnset s where
  pnset ::
    Int
    -- ^ Target node id.
    -> [(String, s Double)]
    -- ^ Parameter name and its values.
    -> s (ToOSC Double)
