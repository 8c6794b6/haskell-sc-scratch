{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable

Parsing patterns encoded in ByteString with Bz.

'parseP' function parses lazy bytestring made with 'lazyByteStringP'
from 'Bz'.  Parsed result could be used as any concrete type which
implements all shown classes.

-}
module Sound.SC3.Lepton.Pattern.ParseP
  ( parseP
  , oPatterns
  , iPatterns
  , dPatterns
  ) where

import Control.Applicative
import Data.Function (fix)
import Prelude hiding (takeWhile)

import Data.Attoparsec.Lazy hiding (takeWhile)
import Data.Attoparsec.Char8 hiding (Result(..), eitherResult, parse)
import Sound.SC3 hiding ((<*), osc)

import qualified Data.ByteString.Char8 as C8

import Sound.SC3.Lepton.Pattern.Dummy ()
import qualified Sound.SC3.Lepton.Pattern.Expression as P

parseP = eitherResult . parse oPatterns

oPatterns = fix (oPatterns' iPatterns)

oPatterns' p f = choice
  [ -- OSC message classes
    psnew, pnset
    -- Primitive
  , pempty
    -- Lists and repeating pattern classes
  , pconcat f, pappend f, pseq p f
  , preplicate p f, pcycle f, pforever f
    -- Random pattern classes
  , pchoose p f
  , prand p f, pshuffle f
    -- Durational pattern classes
  , ptakeT f, pdropT f
    -- Finite state
  , pfsm f
    -- Parallel pattern classes
  , pmerge, ppar
  ]

integral = decimal <|> signed decimal <|> braced (signed decimal)

dPatterns = fix (nPatterns' double iPatterns)
iPatterns = braced (fix (nPatterns' integral iPatterns))

nPatterns' p i f = choice
  [ -- Primitive pattern classes
    pval p, plist p, prepeat p, pempty, prandom
    -- Lists and repeating pattern classes
  , pconcat f, pappend f, pseq i f
  , preplicate i f, pcycle f, pforever f
    -- Random pattern classes
  , prange f, pchoose i f
  , prand i f, pshuffle f
    -- Finite state
  , pfsm f
    -- Num
  , addP f, mulP f, minusP f, negateP f, absP f, signumP f
    -- Fractional
  , divP f, recipP f
    -- Floating
  , piP, expP f, logP f, sqrtP f, powerP f, sinP f, tanP f, cosP f, asinP f
  , atanP f, acosP f, sinhP f, tanhP f, coshP f, asinhP f, atanhP f, acoshP f
    -- UnaryOp
  , ampDbP f, asFloatP f, asIntP f, bitNotP f, cpsMIDIP f, cpsOctP f, cubedP f
  , dbAmpP f, distortP f, fracP f, isNilP f, log10P f, log2P f, midiCPSP f
  , midiRatioP f, notEP f, notNilP f, octCPSP f, ramp_P f, ratioMIDIP f
  , softClipP f, squaredP f
  ]

------------------------------------------------------------------------------
-- Pattern classes

pempty = string "pempty" *> return P.pempty
pval p = mkP "pval" P.pval p
plist p = mkP "plist" P.plist (listOf p)
prepeat p = mkP "prepeat" P.prepeat p
pconcat p = mkP "pconcat" P.pconcat (listOf p)
pappend p = mkP2 "pappend" P.pappend (braced p) (braced p)
pseq p1 p2 = mkP2 "pseq" P.pseq p1 (listOf p2)
preplicate p1 p2 = mkP2 "preplicate" P.preplicate p1 (braced p2)
pcycle p = mkP "pcycle" P.pcycle (listOf p)
pforever p = mkP "pforever" P.pforever (braced p)
prandom = string "prandom" *> return P.prandom
prange p = mkP2 "prange" P.prange (braced p) (braced p)
pchoose p1 p2 = mkP2 "pchoose" P.pchoose p1 (listOf p2)
prand p1 p2 = mkP2 "prand" P.prand p1 (listOf p2)
pshuffle p = mkP "pshuffle" P.pshuffle (listOf p)
ptakeT p = mkP2 "ptakeT" P.ptakeT double (braced p)
pdropT p = mkP2 "pdropT" P.ptakeT double (braced p)
pmerge = mkP2 "pmerge" P.pmerge (braced oPatterns) (braced oPatterns)
ppar = mkP "ppar" P.ppar (listOf oPatterns)
pfsm p = mkP2 "pfsm" P.pfsm (listOf decimal)
  (listOf (braced ((,) <$> p <*> (char ',' *> listOf decimal))))

------------------------------------------------------------------------------
-- OSC message patterns

psnew =
  P.psnew <$> (string "Snew" *> skipSpace *> name) <*>
  (skipSpace *> nodeId) <*> (skipSpace *> addAction) <*>
  (skipSpace *> targetId) <*> (skipSpace *> paramList)

pnset = mkP2 "Nset" P.pnset targetId paramList

nodeId :: Parser (Maybe Int)
nodeId =
  (string "Nothing" *> pure Nothing) <|>
  (braced (string "Just " *> (Just <$> (decimal <|> braced (signed decimal)))))

targetId :: Parser Int
targetId = decimal

name :: Parser String
name = char '"' *> (C8.unpack <$> takeWhile (/= '"')) <* char '"'

addAction :: Parser AddAction
addAction =
  (string "AddToHead" *> pure AddToHead) <|>
  (string "AddToTail" *> pure AddToTail) <|>
  (string "AddBefore" *> pure AddBefore) <|>
  (string "AddAfter" *> pure AddAfter) <|>
  (string "AddReplace" *> pure AddReplace)

paramList = listOf (braced ((,) <$> name <*> (char ',' *> dPatterns)))

------------------------------------------------------------------------------
-- Numeric

addP p    = binOpP (+) (char '+') (braced p)
mulP p    = binOpP (*) (char '*') (braced p)
minusP p  = binOpP (-) (char '-') (braced p)
negateP p = mkP "negate" negate (braced p)
signumP p = mkP "signum" signum (braced p)
absP p    = mkP "abs" abs (braced p)

divP p = binOpP (/) (char '/') (braced p)
recipP p = mkP "recip" recip (braced p)

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
ratioMIDIP p = mkP "ratioMIDI" ratioMIDI (braced p)
softClipP p  = mkP "softClip" softClip (braced p)
squaredP p   = mkP "squared" squared (braced p)

------------------------------------------------------------------------------
-- Utils

mkP n f p1 = string n *> skipSpace *> (f <$> p1)

mkP2 n f p1 p2 = f <$ string n <*> (skipSpace *> p1) <*> (skipSpace *> p2)

binOpP op sym p = op <$> p <*> (skipSpace *> sym *> skipSpace *> p)

braced :: Parser a -> Parser a
braced a = char '(' *> skipSpace *> a <* skipSpace <* char ')'

listOf :: Parser a -> Parser [a]
listOf p = char '[' *> p `sepBy` char ',' <*  char ']'
