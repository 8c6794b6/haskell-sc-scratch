{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

'Bz' for ByteString encoded patterns using Blaze builder.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.Bz
  ( Bz(..)
  , toBz
  , byteStringP
  , lazyByteStringP
  ) where

import Control.Applicative
import Data.Data
import Data.Monoid

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.List.Stream (foldl')
import Sound.SC3

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC

-- | Blaze builder wrapper for patterns.
--
-- Text representation could be converted to ByteString.
--
newtype Bz s = Bz {unBz :: Show s => Builder}

instance Typeable1 Bz where
  typeOf1 _ =
    mkTyConApp (mkTyCon "Sound.SC3.Lepton.Pattern.Interpreter.Bz.Bz") []
instance Typeable a => Typeable (Bz a) where
  typeOf = typeOfDefault

-- | Alias of 'id' to fix type signature.
toBz :: Bz s -> Bz s
toBz = id

-- | Converts 'Bz' to strict ByteString.
byteStringP :: Show s => Bz s -> B.ByteString
byteStringP = toByteString . unBz

-- | Converts 'Bz' to lazy ByteString.
lazyByteStringP :: Show s => Bz s -> BL.ByteString
lazyByteStringP = toLazyByteString . unBz

------------------------------------------------------------------------------
-- Utils

(<>) :: Monoid a => a -> a -> a
a <> b = a `mappend` b
infixl 4 <>
{-# INLINE (<>) #-}

mkOp :: Builder -> Builder -> Builder -> Bz s
mkOp op a b =
  Bz $ fromChar '(' <> a <> fromString ") " <>
  op <>
  fromString " (" <> b <> fromChar ')'
{-# INLINE mkOp #-}

mkUnary :: Builder -> Builder -> Bz s
mkUnary op a = Bz $ op <> fromString " (" <> a <> fromChar ')'
{-# INLINE mkUnary #-}

mkPval :: Show a => a -> Bz s
mkPval a = Bz $ fromString "pval " <> fromString (show a)
{-# INLINE mkPval #-}

mkList :: forall a. Show a => (a -> Builder) -> [a] -> Builder
mkList _ []     = fromString "[]"
mkList f (x:xs) = fromChar '[' <> f x <> go xs where
  go []     = fromChar ']'
  go (y:ys) = fromChar ',' <> f y <> go ys
{-# INLINE mkList #-}

braced :: Builder -> Builder
braced a = fromChar '(' <> a <> fromChar ')'
{-# INLINE braced #-}

space :: Builder
space = fromChar ' '
{-# INLINE space #-}

showFloating :: (Show a, Show b) => String -> Bz a -> Bz b
showFloating f x = Bz (fromString f <> space <> braced (unBz x))
{-# INLINE showFloating #-}

mkParams :: Show t => [(String, Bz t)] -> Builder
mkParams = mkList f where
  f (k,Bz v) = braced (doubleQuote (fromString k) <> fromChar ',' <> v)
{-# INLINE mkParams #-}

doubleQuote :: Builder -> Builder
doubleQuote a = fromChar '"' <> a <> fromChar '"'
{-# INLINE doubleQuote #-}

------------------------------------------------------------------------------
-- Base classes

instance Show a => Show (Bz a) where
  show = C8.unpack . byteStringP

instance Show a => Eq (Bz a) where
  a == b = show a == show b

instance Show a => Ord (Bz a) where
  compare _ _ = EQ

instance Functor Bz where
  fmap _ (Bz z) = Bz (fromString "<Functor>")

instance Applicative Bz where
  pure a  = Bz $ fromString "prepeat " <> fromString (show a)
  _ <*> _ = error "<*> not supported for Bz"

instance (Show a, Enum a) => Enum (Bz a) where
  pred (Bz a) = mkUnary (fromString "pred") a
  succ (Bz a) = mkUnary (fromString "succ") a
  fromEnum n = error "Bz does not support fromEnum"
  toEnum n = mkPval n

------------------------------------------------------------------------------
-- Numeric

instance Num a => Num (Bz a) where
  Bz a + Bz b = mkOp (fromChar '+') a b
  Bz a * Bz b = mkOp (fromChar '*') a b
  Bz a - Bz b = mkOp (fromChar '-') a b
  negate (Bz a) = mkUnary (fromString "negate") a
  abs (Bz a) = mkUnary (fromString "abs") a
  signum (Bz a) = mkUnary (fromString "signum") a
  fromInteger a = Bz $ fromString "pval " <> fromString (show a)

instance Fractional a => Fractional (Bz a) where
  Bz a / Bz b = mkOp (fromChar '/') a b
  fromRational a = Bz $ fromString "pval " <> fromString (show $ fromRational a)

instance Floating a => Floating (Bz a) where
  pi = Bz $ fromString "pi"
  exp = showFloating "exp"
  log = showFloating "log"
  sqrt = showFloating "sqrt"
  Bz a ** Bz b = mkOp (fromString "**") a b
  sin = showFloating "sin"
  tan = showFloating "tan"
  cos = showFloating "cos"
  asin = showFloating "asin"
  atan = showFloating "atan"
  acos = showFloating "acos"
  sinh = showFloating "sinh"
  tanh = showFloating "tanh"
  cosh = showFloating "cosh"
  asinh = showFloating "asinh"
  atanh = showFloating "atanh"
  acosh = showFloating "acosh"

instance UnaryOp a => UnaryOp (Bz a) where
  ampDb = showFloating "ampDb"
  asFloat = showFloating "asFloat"
  asInt = showFloating "asInt"
  bitNot = showFloating "bitNot"
  cpsMIDI = showFloating "cpsMIDI"
  cpsOct = showFloating "cpsOct"
  cubed = showFloating "cubed"
  dbAmp = showFloating "dbAmp"
  distort = showFloating "distort"
  frac = showFloating "frac"
  isNil = showFloating "isNil"
  log10 = showFloating "log10"
  log2 = showFloating "log2"
  midiCPS = showFloating "midiCPS"
  midiRatio = showFloating "midiRatio"
  notE = showFloating "notE"
  notNil = showFloating "notNil"
  octCPS = showFloating "octCPS"
  ramp_ = showFloating "ramp_"
  ratioMIDI = showFloating "ratioMIDI"
  softClip = showFloating "softClip"
  squared = showFloating "squared"

------------------------------------------------------------------------------
-- Primitive patterns

instance Pempty Bz where
  pempty = Bz $ fromString "pempty"

instance Pval Bz where
  pval a = Bz $ fromString "pval " <> fromString (show a)

instance Prepeat Bz where
  prepeat a = Bz $ fromString "prepeat " <> fromString (show a)

instance Plist Bz where
  plist as = Bz $ fromString "plist " <> mkList (fromString . show) as

------------------------------------------------------------------------------
-- Lists and repeat patterns

instance Pappend Bz where
  pappend (Bz a) (Bz b) =
    Bz $ fromString "pappend " <> braced a <> space <> braced b

instance Pseq Bz where
  pseq (Bz n) ps =
    Bz $ fromString "pseq " <> braced n <> space <> mkList unBz ps

instance Preplicate Bz where
  preplicate (Bz n) (Bz p) =
    Bz $ fromString "preplicate " <> braced n <> space <> braced p

instance Pconcat Bz where
  pconcat ps = Bz $ fromString "pconcat " <> mkList unBz ps

instance Pforever Bz where
  pforever (Bz p) = Bz $ fromString "pforever " <> braced p

instance Pcycle Bz where
  pcycle ps = Bz $ fromString "pcycle " <> mkList unBz ps

------------------------------------------------------------------------------
-- Random patterns

instance Prand Bz where
  prand (Bz n) ps =
    Bz $ fromString "prand " <> braced n <> space <> mkList unBz ps

instance Prange Bz where
  prange (Bz a) (Bz b) =
    Bz $ fromString "prange " <> braced a <> space <> braced b

instance Prandom Bz where
  prandom = Bz $ fromString "prandom"

instance Pshuffle Bz where
  pshuffle ps =
    Bz $ fromString "pshuffle " <> mkList unBz ps

instance Pchoose Bz where
  pchoose (Bz n) ps =
    Bz $ fromString "pchoose " <> braced n <> space <> mkList unBz ps

------------------------------------------------------------------------------
-- Parallel patterns

instance Show a => Mergable (Bz a) where
  merge (Bz a) (Bz b) = Bz $ fromString "merge " <> a <> b

instance Pmerge Bz where
  pmerge (Bz a) (Bz b) = Bz $ fromString "pmerge " <> braced a <> space <> braced b

instance Ppar Bz where
  ppar ps = Bz $ fromString "ppar " <> mkList unBz ps

------------------------------------------------------------------------------
-- OSC message patterns

instance Psnew Bz where
  psnew def nid aa tid ms =
    Bz $ fromString (show $ Snew def nid aa tid) <> space <> mkParams ms

instance Pnset Bz where
  pnset nid ms =
    Bz $ fromString (show $ Nset nid) <> space <> mkParams ms
