{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

'S' for showing patterns.

XXX: /Rewrite with using blaze-builder/.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.S where

import Control.Applicative
import Data.Data
import Text.Show.Functions ()

import Sound.SC3

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Data.Map as M

-- | \"S\" for showing patterns.
--
-- Enumeration for floating points are not working here also.
-- fromEnum and toEnum are assuming pval only.
--
newtype S s = S {unS :: forall a. (Show s) => a -> String}

-- | Show string representation of pattern.
showP :: (Show a) => S a -> String
showP p = unS p ()

instance (Show a, Eq a) => Eq (S a) where
  a == b = showP a == showP b

instance (Show a) => Show (S a) where
  show = showP

instance Typeable1 S where
  typeOf1 _ = mkTyConApp (mkTyCon "Sound.SC3.Lepton.Pattern.S") []

-- | Plain numbers would be shown as pval.
instance (Num a) => Num (S a) where
  a + b = S $ \_ -> showP a ++ " + " ++ showP b
  a * b = S $ \_ -> showP a ++ " * " ++ showP b
  abs n = S $ \_ -> "abs (" ++ showP n ++ ")"
  negate n = S $ \_ -> "negate (" ++ showP n ++ ")"
  signum n = S $ \_ -> "signum (" ++ showP n ++ ")"
  fromInteger n = S $ \_ -> "pval " ++ show (fromInteger n :: Int)

instance (Fractional a) => Fractional (S a) where
  a / b = S $ \_ -> showP a ++ " / " ++ showP b
  fromRational n = S $ \_ -> "pval " ++ show (fromRational n :: Double)

instance (Show a, Enum a) => Enum (S a) where
  pred n = S $ \_ -> "pred (" ++ showP n ++ ")"
  succ n = S $ \_ -> "succ (" ++ showP n ++ ")"
  fromEnum n = case words $ showP n of
    [x]         -> read x
    ["pval", x] -> fromEnum (read x :: Double) -- XXX: how to tell the type?
    e           -> error $ "fromEnum: " ++ show e
  toEnum n = S $ \_ -> "pval " ++ show n

instance Functor S where
  fmap f (S _) = S (\_ -> show f)

instance Applicative S where
  pure a = S (\_ -> show a)
  S f <*> S _ = S (\_ -> show f)

instance (Show a, Ord a) => Ord (S a) where
  compare _ _ = EQ

instance Floating a => Floating (S a) where
  pi = S (const "pi")
  exp = showFloating "exp"
  log = showFloating "log"
  sqrt = showFloating "sqrt"
  a ** b = S (const $ show a ++ " ** " ++ show b)
  sin = showFloating "sin"
  cos = showFloating "cos"
  asin = showFloating "asin"
  atan = showFloating "atan"
  acos = showFloating "acos"
  sinh = showFloating "sinh"
  cosh = showFloating "cosh"
  asinh = showFloating "asinh"
  atanh = showFloating "atanh"
  acosh = showFloating "acosh"

showFloating :: Show a => String -> a -> S s
showFloating f x = S (const $ f ++ " (" ++ show x ++ ")")

instance UnaryOp a => UnaryOp (S a) where
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


--
-- Instance for expressions
--

instance Pval S where
  pval a = S $ \_ -> "pval " ++ show a

instance Pempty S where
  pempty = S $ \_ -> "pempty"

instance Plist S where
  plist a = S $ \_ -> "plist " ++ show a

instance Pconcat S where
  pconcat p = S $ \_ -> "pconcat " ++ showList p ""

instance Pappend S where
  pappend a b = S $ \_ -> "pappend (" ++ showP a ++ ") (" ++ showP b ++ ")"

instance Pseq S where
  pseq n p = S $ \_ -> "pseq (" ++ showP n ++ ") " ++ showList p ""

instance Preplicate S where
  preplicate n p = S $ \_ -> "preplicate (" ++ showP n ++ ") (" ++ showP p ++ ")"

instance Prand S where
  prand n p = S $ \x -> "prand (" ++ unS n x ++ ") " ++ showList p ""

instance Prange S where
  prange lo hi = S $ \_ -> "prange (" ++ showP lo ++ ") (" ++ showP hi ++ ")"

instance Prandom S where
  prandom = S $ \_ -> "prandom"

instance Pshuffle S where
  pshuffle p = S $ \_ -> "pshuffle " ++ showList p ""

instance Pchoose S where
  pchoose n p = S $ \_ -> "pchoose (" ++ showP n ++ ") " ++ showList p ""

instance Pcycle S where
  pcycle p = S $ \_ -> "pcycle " ++ showList p ""

instance Prepeat S where
  prepeat a = S $ \_ -> "prepeat " ++ show a

instance Pforever S where
  pforever p = S $ \_ -> "pforever (" ++ show p ++ ")"

instance Papp S where
  papp _ _ = S $ \_ -> "papp "

instance Pmerge S where
  pmerge a b = S (\_ -> "pmerge (" ++ showP a ++ ") (" ++ showP b ++ ")")

instance Ppar S where
  ppar ps = S (\_ -> "ppar " ++ showList ps "")

instance Show a => Mergable (S a) where
  merge a b = S (\_ -> unwords ["merge", show a, show b])

instance Psnew S where
  psnew def nid aa tid ms =
    S (\_ -> show $ ToOSC (Snew def nid aa tid) (M.fromList ms))

instance Pnset S where
  pnset i ms = S (\_ -> show $ ToOSC (Nset i) (M.fromList ms))

-- instance Plam S where
--   plam f = S $ \_ -> "\\x -> " ++ unS (f (S $ const "")) () ++ ")"

-- instance Papp S where
--   papp a b = S $ \x -> "(" ++ unS a x ++ " " ++ unS b x ++ ")"
