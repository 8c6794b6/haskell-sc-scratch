{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Text.Show.Functions ()

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Sound.SC3

import qualified Blaze.ByteString.Builder.Char8 as BzC8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.String as DS

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC

default (Integer,Double)

-- | Blaze builder wrapper for patterns.
--
-- Text representation could be converted to ByteString.
--
newtype Bz s = Bz {unBz :: Show s => VarIdx -> Builder}

type VarIdx = Int

instance DS.IsString Builder where
  fromString = BzC8.fromString

instance Typeable1 Bz where
  typeOf1 _ =
    mkTyConApp (mkTyCon "Sound.SC3.Lepton.Pattern.Interpreter.Bz.Bz") []

instance Typeable a => Typeable (Bz a) where
  typeOf = typeOfDefault

-- | Alias of 'id' to fix type signature.
toBz :: Bz s -> Bz s
toBz = id

builderP :: forall s. Show s => Bz s -> Builder
builderP bz = unBz bz 0

-- | Converts 'Bz' to strict ByteString.
byteStringP :: Show s => Bz s -> B.ByteString
byteStringP = toByteString . builderP

-- | Converts 'Bz' to lazy ByteString.
lazyByteStringP :: Show s => Bz s -> BL.ByteString
lazyByteStringP = toLazyByteString . builderP

------------------------------------------------------------------------------
-- Utils

(<>) :: Monoid a => a -> a -> a
a <> b = a `mappend` b
infixl 4 <>
{-# INLINE (<>) #-}

mkOp :: Builder -> (VarIdx -> Builder) -> (VarIdx -> Builder) -> Bz s
mkOp op a b = -- undefined
  Bz $ \h -> fromChar '(' <> a h <> ") " <> op <> " (" <> b h <> fromChar ')'
{-# INLINE mkOp #-}

mkUnary :: Builder -> (VarIdx -> Builder) -> Bz s
mkUnary op a = Bz $ \h -> op <> space <> braced (a h)
{-# INLINE mkUnary #-}

mkList :: forall a. Show a => (a -> Builder) -> [a] -> Builder
mkList _ []     = "[]"
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
showFloating f x = Bz (\h -> fromString f <> space <> braced (unBz x h))
{-# INLINE showFloating #-}

mkParams :: Show t => VarIdx -> [(String, Bz t)] -> Builder
mkParams h = mkList f where
  f (k,Bz v) = braced (doubleQuote (fromString k) <> fromChar ',' <> v h)
{-# INLINE mkParams #-}

doubleQuote :: Builder -> Builder
doubleQuote a = fromChar '"' <> a <> fromChar '"'
{-# INLINE doubleQuote #-}

-- ------------------------------------------------------------------------------
-- -- Base classes

instance Show a => Show (Bz a) where
  show = C8.unpack . byteStringP

instance Show a => Eq (Bz a) where
  a == b = show a == show b

instance Show a => Ord (Bz a) where
  compare _ _ = EQ

instance Functor Bz where
  fmap _ (Bz _) = Bz (const "<Functor>")

instance Applicative Bz where
  pure a  = Bz $ const $ "prepeat " <> fromString (show a)
  _ <*> _ = error "<*> not supported for Bz"

instance (Show a, Enum a) => Enum (Bz a) where
  pred (Bz a) = mkUnary ("pred") a
  succ (Bz a) = mkUnary ("succ") a
  fromEnum _ = error "Bz does not support fromEnum"
  toEnum n = Bz $ const $ "pval " <> (fromString $ show (toEnum n :: Double))

------------------------------------------------------------------------------
-- Numeric

instance Num a => Num (Bz a) where
  Bz a + Bz b = mkOp (fromChar '+') a b
  Bz a * Bz b = mkOp (fromChar '*') a b
  Bz a - Bz b = mkOp (fromChar '-') a b
  negate (Bz a) = mkUnary "negate" a
  abs (Bz a) = mkUnary "abs" a
  signum (Bz a) = mkUnary "signum" a
  fromInteger a
    | a < 0 = Bz $ \_ -> "pval (" <>
         fromString (show (fromInteger a :: Integer)) <> fromChar ')'
    | otherwise = Bz $ \_ -> fromString "pval " <>
         fromString (show (fromInteger a :: Integer))

instance Fractional a => Fractional (Bz a) where
  Bz a / Bz b = mkOp (fromChar '/') a b
  recip (Bz a) = mkUnary "recip" a
  fromRational a
    | a < 0 = Bz $ \_ ->
      "pval (" <> fromString (show (fromRational a :: Double)) <> fromChar ')'
    | otherwise = Bz $ \_ ->
      "pval " <> fromString (show (fromRational a :: Double))

instance Floating a => Floating (Bz a) where
  pi = Bz $ const "pi"
  exp = showFloating "exp"
  log = showFloating "log"
  sqrt = showFloating "sqrt"
  Bz a ** Bz b = mkOp ("**") a b
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
  pempty = Bz $ const "pempty"

instance Pval Bz where
  pval a = Bz $ const $ "pval " <> fromString (show a)

instance Prepeat Bz where
  prepeat a = Bz $ const $ "prepeat " <> fromString (show a)

instance Plist Bz where
  plist as = Bz $ const $ "plist " <> mkList (fromString . show) as

------------------------------------------------------------------------------
-- Lists and repeat patterns

instance Pappend Bz where
  pappend (Bz a) (Bz b) =
    Bz $ \h -> "pappend " <> braced (a h) <> space <> braced (b h)

instance Pseq Bz where
  pseq (Bz n) ps =
    Bz $ \h -> "pseq " <> braced (n h) <> space <> mkList (flip unBz h) ps

instance Preplicate Bz where
  preplicate (Bz n) (Bz p) =
    Bz $ \h -> "preplicate " <> braced (n h) <> space <> braced (p h)

instance Pconcat Bz where
  pconcat ps = Bz $ \h -> "pconcat " <> mkList (flip unBz h) ps

instance Pforever Bz where
  pforever (Bz p) = Bz $ \h -> "pforever " <> braced (p h)

instance Pcycle Bz where
  pcycle ps = Bz $ \h -> "pcycle " <> mkList (flip unBz h) ps

------------------------------------------------------------------------------
-- Random patterns

instance Prand Bz where
  prand (Bz n) ps =
    Bz $ \h -> "prand " <> braced (n h) <> space <> mkList (flip unBz h) ps

instance Prange Bz where
  prange (Bz a) (Bz b) =
    Bz $ \h -> "prange " <> braced (a h) <> space <> braced (b h)

instance Prandom Bz where
  prandom = Bz $ const $ "prandom"

instance Pshuffle Bz where
  pshuffle ps = Bz $ \h -> "pshuffle " <> mkList (flip unBz h) ps

instance Pchoose Bz where
  pchoose (Bz n) ps =
    Bz $ \h -> "pchoose " <> braced (n h) <> space <> mkList (flip unBz h) ps

------------------------------------------------------------------------------
-- Parallel patterns

instance Show a => Mergable (Bz a) where
  merge (Bz a) (Bz b) = Bz $ \h -> "merge " <> a h <> b h

instance Pmerge Bz where
  pmerge (Bz a) (Bz b) =
    Bz $ \h -> "pmerge " <> braced (a h) <> space <> braced (b h)

instance Ppar Bz where
  ppar ps = Bz $ \h -> "ppar " <> mkList (flip unBz h) ps

------------------------------------------------------------------------------
-- Durational patterns

instance PtakeT Bz where
  ptakeT t (Bz a) =
    Bz $ \h -> "ptakeT " <> fromString (show t) <> space <> braced (a h)

instance PdropT Bz where
  pdropT t (Bz a) =
    Bz $ \h -> "pdropT " <> fromString (show t) <> space <> braced (a h)

------------------------------------------------------------------------------
-- Finite state pattern

instance Pfsm Bz where
  pfsm is cs = Bz $ \h -> "pfsm " <> mkList f is <> space <> mkList (g h) cs
    where
      f = fromString . show
      g h (p,js) = braced (unBz p h <> fromChar ',' <> mkList f js)

------------------------------------------------------------------------------
-- Lambda

instance Plam Bz where
  plam f = Bz $ \h ->
    case "x" <> fromString (show h) of
      x -> case mkList (flip unBz (succ h)) (f (Bz $ const x)) of
        body -> "plam (\\" <> x <> " -> " <> body <> ")"

instance Papp Bz where
  papp f e = Bz $ \h ->
    "papp" <> space <> braced (unBz f h) <> space <> braced (unBz e h)

------------------------------------------------------------------------------
-- OSC message patterns

instance Psnew Bz where
  psnew def nid aa tid ms =
    Bz $ \h -> fromString (show $ Snew def nid aa tid) <> space <> mkParams h ms

instance Pnset Bz where
  pnset nid ms =
    Bz $ \h -> fromString (show $ Nset nid) <> space <> mkParams h ms
