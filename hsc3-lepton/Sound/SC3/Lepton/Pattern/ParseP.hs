{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (OverloadedStrings)

Parsing patterns encoded in ByteString with Bz.

'parseP' function parses lazy bytestring made with 'lazyByteStringP'
from 'Bz'.  Parsed result could be used as any concrete type which
implements all shown classes.

-}
module Sound.SC3.Lepton.Pattern.ParseP ( parseP ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Function (fix)
import Prelude hiding (takeWhile)
import System.Random

import Data.Attoparsec.Lazy hiding (takeWhile)
import Data.Attoparsec.Char8 hiding (Result(..), eitherResult, parse)
import Data.Attoparsec.Combinator
import Sound.SC3 hiding ((<*), osc)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Map as M

import qualified Sound.SC3.Lepton.Pattern.Expression as P

parseP = eitherResult . parse rPatterns

rPatterns = choice
  [ -- OSC message classes
    psnew, pnset
    -- Primitive
  , pempty
    -- Lists and repeating pattern classes
  , pconcat rPatterns, pappend rPatterns, pseq intP rPatterns
  , preplicate intP rPatterns, pcycle rPatterns, pforever rPatterns
    -- Random pattern classes
  , pchoose intP rPatterns
  , prand intP rPatterns, pshuffle rPatterns
    -- Parallel pattern classes
  , pmerge, ppar
  ]

nPatterns = fix nPatterns'

nPatterns' f = choice
  [ -- Primitive pattern classes
    pval number, plist number, prepeat number, pempty, prandom
    -- Lists and repeating pattern classes
  , pconcat f, pappend f, pseq intP f
  , preplicate intP f, pcycle f, pforever f
    -- Random pattern classes
  , prange f, pchoose intP f
  , prand intP f, pshuffle f
    -- Num
  , addP f, mulP f, minusP f, negateP f, absP f, signumP f
    -- Fractional
  , divP f
    -- Floating
  , piP, expP f, logP f, sqrtP f, powerP f, sinP f, tanP f, cosP f, asinP f
  , atanP f, acosP f, sinhP f, tanhP f, coshP f, asinhP f, atanhP f, acoshP f
    -- UnaryOp
  , ampDbP f, asFloatP f, asIntP f, bitNotP f, cpsMIDIP f, cpsOctP f, cubedP f
  , dbAmpP f, distortP f, fracP f, log10P f, log2P f, midiCPSP f, midiRatioP f
  , notEP f, notNilP f, octCPSP f, ramp_P f, ratioMIDIP f, softClipP f
  , squaredP f
  ]

------------------------------------------------------------------------------
-- Primitives

pempty    = string "pempty" *> return P.pempty
pval p    = mkP "pval" P.pval p
plist p   = mkP "plist" P.plist (listOf p)
prepeat p = mkP "prepeat" P.prepeat p

------------------------------------------------------------------------------
-- Looping patterns

pconcat p        = mkP "pconcat" P.pconcat (listOf p)
pappend p        = mkP2 "pappend" P.pappend (braced p) (braced p)
pseq p1 p2       = mkP2 "pseq" P.pseq p1 (listOf p2)
preplicate p1 p2 = mkP2 "preplicate" P.preplicate p1 (braced p2)
pcycle p         = mkP "pcycle" P.pcycle (listOf p)
pforever p       = mkP "pforever" P.pforever (braced p)

------------------------------------------------------------------------------
-- Random patterns

prandom       = string "prandom" *> return P.prandom
prange p      = mkP2 "prange" P.prange (braced p) (braced p)
pchoose p1 p2 = mkP2 "pchoose" P.pchoose p1 (listOf p2)
prand p1 p2   = mkP2 "prand" P.prand p1 (listOf p2)
pshuffle p    = mkP "pshuffle" P.pshuffle (listOf p)

------------------------------------------------------------------------------
-- Parallel patterns

pmerge = mkP2 "pmerge" P.pmerge (braced rPatterns) (braced rPatterns)
ppar   = mkP "ppar" P.ppar (listOf rPatterns)

------------------------------------------------------------------------------
-- OSC message patterns

psnew = do
  string "Snew"
  name' <- skipSpace *> name
  nodeId' <- skipSpace *> nodeId
  addAction' <- skipSpace *> addAction
  targetId' <- skipSpace *> targetId
  paramList'  <- skipSpace *> paramList
  return $ P.psnew name' nodeId' addAction' targetId' paramList'

pnset = mkP2 "Nset" P.pnset targetId paramList

-- XXX:
-- This function is making the result type to be an instance of Functor.
paramList = listOf (braced pair) where
  pair = (,) <$> name <*> (char ',' *> (fmap n2double <$> nPatterns))

nodeId :: Parser (Maybe Int)
nodeId =
  (string "Nothing" *> pure Nothing) <|>
  (braced (string "Just " *> (Just . n2int <$> (number <|> braced number))))

targetId :: Parser Int
targetId = n2int <$> number

name :: Parser String
name = char '"' *> (C8.unpack <$> takeWhile (/= '"')) <* char '"'

addAction :: Parser AddAction
addAction =
  (string "AddToHead" *> pure AddToHead) <|>
  (string "AddToTail" *> pure AddToTail) <|>
  (string "AddBefore" *> pure AddBefore) <|>
  (string "AddAfter" *> pure AddAfter) <|>
  (string "AddReplace" *> pure AddReplace)

------------------------------------------------------------------------------
-- Num

addP p    = binOpP (+) (char '+') (braced p)
mulP p    = binOpP (*) (char '*') (braced p)
minusP p  = binOpP (-) (char '-') (braced p)
negateP p = mkP "negate" negate (braced p)
signumP p = mkP "signum" signum (braced p)
absP p    = mkP "abs" abs (braced p)

------------------------------------------------------------------------------
-- Fractional

divP p = binOpP (/) (char '/') (braced p)

-----------------------------------------------------------------------------
-- Floating

piP      = string "pi" *> pure pi
expP p   = mkP "exp" exp (braced p)
logP p   = mkP "log" log (braced p)
sqrtP p  = mkP "sqrt" sqrt (braced p)
powerP p = binOpP (**) (string "**") (braced p)
sinP p   = mkP "sin" sin (braced p)
tanP p   = mkP "tan" tan (braced p)
cosP p   = mkP "cos" cos (braced p)
asinP p  = mkP "asin" asin (braced p)
atanP p  = mkP "atan" atan (braced p)
acosP p  = mkP "acos" acos (braced p)
sinhP p  = mkP "sinh" sinh (braced p)
tanhP p  = mkP "tanh" tanh (braced p)
coshP p  = mkP "cosh" cosh (braced p)
asinhP p = mkP "asinh" asinh (braced p)
atanhP p = mkP "atanh" atanh (braced p)
acoshP p = mkP "acosh" acosh (braced p)

------------------------------------------------------------------------------
-- Unary

ampDbP p     = mkP "ampDb" ampDb (braced p)
asFloatP p   = mkP "asFloat" asFloat (braced p)
asIntP p     = mkP "asInt" asInt (braced p)
bitNotP p    = mkP "bitNot" bitNot (braced p)
cpsMIDIP p   = mkP "cpsMIDI" cpsMIDI (braced p)
cpsOctP p    = mkP "cpsOct" cpsOct (braced p)
cubedP p     = mkP "cubed" cubed (braced p)
dbAmpP p     = mkP "dbAmp" dbAmp (braced p)
distortP p   = mkP "distort" distort (braced p)
fracP p      = mkP "frac" frac (braced p)
isNilP p     = mkP "isNil" isNil (braced p)
log10P p     = mkP "log10" log10 (braced p)
log2P p      = mkP "log2" log2 (braced p)
midiCPSP p   = mkP "midiCPS" midiCPS (braced p)
midiRatioP p = mkP "midiRatio" midiRatio (braced p)
notEP p      = mkP "notE" notE (braced p)
notNilP p    = mkP "notNil" notNil (braced p)
octCPSP p    = mkP "octCPS" octCPS (braced p)
ramp_P p     = mkP "ramp_" ramp_ (braced p)
ratioMIDIP p = mkP "ratioMIDIP" ratioMIDI (braced p)
softClipP p  = mkP "softClip" softClip (braced p)
squaredP p   = mkP "squared" squared (braced p)

------------------------------------------------------------------------------
-- Utils

mkP n f p1 = string n *> skipSpace *> (f <$> p1)

mkP2 n f p1 p2 = f <$ string n <*> (skipSpace *> p1) <*> (skipSpace *> p2)

binOpP op sym p = op <$> p <*> (skipSpace *> sym *> skipSpace *> p)

intP = fmap n2int <$> braced (nPatterns)

braced :: Parser a -> Parser a
braced a = char '(' *> skipSpace *> a <* skipSpace <* char ')'

listOf :: Parser a -> Parser [a]
listOf p = char '[' *> p `sepBy` char ',' <*  char ']'

n2int :: Number -> Int
n2int n = case n of
  I i  -> fromIntegral i
  D d  -> truncate d

n2double :: Number -> Double
n2double n = case n of
  I i -> fromIntegral i
  D d -> d

------------------------------------------------------------------------------
-- Instance definitions for Number

instance Random Number where
  random g = case random g of (n, g') -> (D n,g')
  randomR (I lo, I hi) g = case randomR (lo, hi) g of (n,g') -> (I n, g')
  randomR (D lo, D hi) g = case randomR (lo, hi) g of (n,g') -> (D n, g')
  randomR _ _ = error "Number with mismatched constructor in randomR"

instance Floating Number where
  pi = D pi
  exp (D a) = D (exp a)
  exp (I a) = D (exp $ fromIntegral a)
  sqrt (D a) = D (sqrt a)
  sqrt (I a) = D (sqrt $ fromIntegral a)
  log (D a) = D (log a)
  log (I a) = D (log $ fromIntegral a)
  (D a) ** (D b) = D (a ** b)
  (D a) ** (I b) = D (a ** fromIntegral b)
  (I a) ** (D b) = D (fromIntegral a ** b)
  (I a) ** (I b) = D (fromIntegral a ** fromIntegral b)
  logBase (D a) (D b) = D (logBase a b)
  logBase (D a) (I b) = D (logBase a (fromIntegral b))
  logBase (I a) (D b) = D (logBase (fromIntegral a) b)
  logBase (I a) (I b) = D (logBase (fromIntegral a) (fromIntegral b))
  sin (D a) = D (sin a)
  sin (I a) = D (sin $ fromIntegral a)
  tan (D a) = D (tan a)
  tan (I a) = D (tan $ fromIntegral a)
  cos (D a) = D (cos a)
  cos (I a) = D (cos $ fromIntegral a)
  asin (D a) = D (asin a)
  asin (I a) = D (asin $ fromIntegral a)
  atan (D a) = D (atan a)
  atan (I a) = D (atan $ fromIntegral a)
  acos (D a) = D (acos a)
  acos (I a) = D (acos $ fromIntegral a)
  sinh (D a) = D (sinh a)
  sinh (I a) = D (sinh $ fromIntegral a)
  tanh (D a) = D (tanh a)
  tanh (I a) = D (tanh $ fromIntegral a)
  cosh (D a) = D (cosh a)
  cosh (I a) = D (cosh $ fromIntegral a)
  asinh (D a) = D (asinh a)
  asinh (I a) = D (asinh $ fromIntegral a)
  atanh (D a) = D (atanh a)
  atanh (I a) = D (atanh $ fromIntegral a)
  acosh (D a) = D (acosh a)
  acosh (I a) = D (acosh $ fromIntegral a)

instance UnaryOp Number
