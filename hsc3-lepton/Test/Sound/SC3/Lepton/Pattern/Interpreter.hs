{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

-}
module Test.Sound.SC3.Lepton.Pattern.Interpreter where

import Control.Applicative
import Control.DeepSeq
import Data.Binary (decode, encode)
import Data.Data
import Data.Ratio ((%))
import Test.QuickCheck

import Sound.SC3
import Sound.SC3.Lepton.Pattern
import Sound.SC3.Lepton.Pattern.Interpreter.Expr
import Sound.SC3.Lepton.QuickCheck
import System.Random.Mersenne.Pure64

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize as Srl

tests :: [Property]
tests =
  [ label "prop_prim_num" prop_prim_num
  , label "prop_listpat" prop_listpat
  , label "prop_fractional" prop_fractional
  , label "prop_floating" prop_floating
  , label "prop_unary" prop_unary
  , label "prop_fromnum" prop_fromnum
  , label "prop_outer_repeat" prop_outer_repeat
  ]

prop_listpat = with_ppar_snew_nset list_patterns

prop_unary = with_ppar_snew_nset unary_patterns

prop_prim_num = with_ppar_snew_nset num_patterns

prop_floating = with_ppar_snew_nset floating_patterns

prop_fractional = with_ppar_snew_nset fractional_patterns

prop_fromnum =
  forAll (arbitrary `suchThat` 
          (\(_,n::Integer,d::Integer) -> d /= 0 && n /= 0)) $
  \(i::Int,n::Integer,d::Integer) ->
  let m = n % d
  in  fromRational m == toExpr (pval m) &&
      fromIntegral i == toExpr (pval i) &&
      fromRational m == toBz (fromRational m) &&
      fromIntegral i == toBz (fromIntegral i) &&
      fromRational m == toS (fromRational m) &&
      fromIntegral i == toS (fromIntegral i)
      
prop_outer_repeat = forAll mixed_patterns $ \p ->
  let p1 = toExpr (preplicate 2 (pmerge (snew_foo p) (nset_100 p)))
      p2 = toExpr (pseq (prange 1 4) [snew_foo p, nset_100 p])
      Right p1' = fromExpr p1
      Right p2' = fromExpr p2
  in  p1' == p1 && p2' == p2

with_ppar_snew_nset ps = forAll ps $ \p ->
  let p' = pconcat [ ppar [ snew_foo p, nset_100 p ]
                   , pmerge (snew_foo p) (nset_100 p) ]
      Right (b :: Bz (ToOSC Double)) = fromExpr p'
      Right (s :: S (ToOSC Double)) = fromExpr p'
      Right (e :: Expr (ToOSC Double)) = fromExpr p'
  in  check_expr e && check_bz_s b s

check_expr e = e `deepseq`
  let Right eb = fromExpr (decode (encode $ toExpr e))
      Right es = fromExpr =<< Srl.decode (Srl.encode $ toExpr e)
      eString = fmap show e
  in  eb == e && es == e &&
      eString `deepseq` not (null (show $ prettyP eString)) &&
      typeOf e == typeOf (undefined :: Expr (ToOSC Double))

check_bz_s bz s =
  show (toBz bz) == show (toS s) &&
  bz == bz && lazyByteStringP bz == BSL.fromChunks [byteStringP bz] &&
  typeOf bz == typeOf (undefined :: Bz (ToOSC Double)) &&
  s == s

------------------------------------------------------------------------------
-- Pattern generators

snew_foo o = psnew "foo" Nothing AddToTail 1 [("bar", o)]

nset_100 o = pnset 100 [("bar", o)]

prims = \(a,b,c) -> [pempty, pval a, plist b, prepeat c]

list_patterns = make_patterns $ \p1 p2 pi ->
  [ pconcat [p1,p2], pappend p1 p2, pcycle [p1,p2]
  , pseq pi [p1,p2], preplicate pi p1, pforever p1 ]

random_patterns = make_patterns $ \p1 p2 pi ->
  [ prandom, prange p1 p2, pchoose pi [p1,p2]
  , prand pi [p1,p2], pshuffle [p1,p2] ]

num_patterns =
  elements [\x -> x + x, \x -> x * x, \x -> x - x, abs, negate, signum] <*>
  mixed_patterns

unary_patterns =
  (elements
    [ ampDb, asFloat, asInt, bitNot, cpsMIDI, cpsOct, cubed, dbAmp
    , distort, frac, isNil, log10, log2, midiCPS, midiRatio
    , notE, notNil, octCPS, ramp_, ratioMIDI, softClip, squared ]) <*>
  (elements =<< sequence [list_patterns, random_patterns])

fractional_patterns =
  elements [ \x -> x / x, recip ] <*>
  (elements =<< sequence [list_patterns, random_patterns])

floating_patterns =
  elements
    [ const pi, exp, sqrt, log, \x -> x ** x, \x -> logBase x x
    , sin, cos, tan, asin, atan, acos, sinh, tanh, cosh, asinh, atanh, acosh ] <*>
  (elements =<< sequence [list_patterns, random_patterns])

mixed_patterns = elements =<< sequence
  [ list_patterns, random_patterns, unary_patterns
  , fractional_patterns, floating_patterns ]

make_patterns f = do
  (x,y,z) <- arbitrary
  elements =<< (f <$> (elements (prims x)) <*>
                (elements (prims y)) <*>
                (elements (prims z)))
