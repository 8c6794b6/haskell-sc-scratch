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
module Test.Sound.SC3.Lepton.Pattern where

import Control.Applicative
import Data.Binary (decode, encode)
import Test.QuickCheck

import Sound.SC3
import Sound.SC3.Lepton.Pattern
import Sound.SC3.Lepton.Pattern.Interpreter.Expr
import Sound.SC3.Lepton.QuickCheck
import System.Random.Mersenne.Pure64

-- import qualified Data.ByteString.Char8 as C8
-- import qualified Data.ByteString.Lazy.Char8 as LC8

tests :: [Property]
tests =
  [
  -- Fails with `Functor not supported for Bz.`
  -- , label "prop_num_bz" prop_num_bz
    label "prop_prim_num" prop_prim_num
  , label "prop_listpat" prop_listpat
  , label "prop_fractional" prop_fractional
  , label "prop_floating" prop_floating
  , label "prop_unary" prop_unary
  ]

prop_prim_num = forAll arbitrary $
  \((x,y) :: ((Double,[Double],Double)
             ,(Double,Double,Double,Integer,Double))) ->
  let p = ppar [ snew_foo (prim_patterns x)
               , nset_100 (num_patterns y) ]
      -- XXX: Cabal test fails to compile this.
      -- Dup e0 b = duplicate p
      -- Dup e1 s = duplicate e0
      -- Dup e2 r = duplicate e1
      Right (b::Bz (ToOSC Double))    = fromExpr p
      Right (s::S (ToOSC Double))     = fromExpr p
      Right (r::R (ToOSC Double))     = fromExpr p
      Right (e::Expr (ToOSC Double)) = fromExpr p
  in  check_expr e && check_bz_s b s && check_r r

prop_listpat = forAll list_patterns $ \p ->
  let p' = ppar [ snew_foo p, nset_100 p ]
      Right (b::Bz (ToOSC Double)) = fromExpr p'
      Right (s::S (ToOSC Double)) = fromExpr p'
      Right (r::R (ToOSC Double)) = fromExpr p'
      Right (e::Expr (ToOSC Double)) = fromExpr p'
  in  check_expr e && check_bz_s b s && check_r r

prop_unary = with_ppar_snew_nset unary_patterns

prop_floating = with_ppar_snew_nset floating_patterns

prop_fractional = with_ppar_snew_nset fractional_patterns

with_ppar_snew_nset ps = forAll ps $ \p ->
  let p' = pconcat [ ppar [ snew_foo p, nset_100 p ]
                   , pmerge (snew_foo p) (nset_100 p) ]
      Right (b :: Bz (ToOSC Double)) = fromExpr p'
      Right (s :: S (ToOSC Double)) = fromExpr p'
      Right (r :: R (ToOSC Double)) = fromExpr p'
      Right (e :: Expr (ToOSC Double)) = fromExpr p'
  in  check_expr e && check_bz_s b s -- && check_r r

check_expr e = decode (encode $ toExpr e) == e

check_bz_s bz s = show bz == show s

check_r r =
  let r' = take 20 $ runP r (pureMT 0)
  in  r' `seq` (null r' || not (null r'))

------------------------------------------------------------------------------
-- Pattern generators

snew_foo o = psnew "foo" Nothing AddToTail 1 [("bar", o)]

nset_100 o = pnset 100 [("bar", o)]

prim_patterns = \x -> pconcat (prims x)

prims = \(a,b,c) -> [pempty, pval a, plist b, prepeat c]

non_empty_prims = \(a,b,c) -> [pval a, plist b, prepeat c]

num_patterns = \(a,b,c,d,e) ->
  abs ((pval a + signum (pval c) *
        pval b - (fromInteger d) + (negate (pval e))))

list_patterns = make_patterns $ \p1 p2 pi ->
  [ pconcat [p1,p2], pappend p1 p2, pcycle [p1,p2]
  , pseq pi [p1,p2], preplicate pi p1, pforever p1 ]

random_patterns = make_patterns $ \p1 p2 pi ->
  [ prandom, prange p1 p2, pchoose pi [p1,p2]
  , prand pi [p1,p2], pshuffle [p1,p2] ]

unary_patterns = do
  fun <- elements
         [ ampDb, asFloat, asInt, bitNot, cpsMIDI, cpsOct, cubed, dbAmp
         , distort, frac, isNil, log10, log2, midiCPS, midiRatio
         , notE, notNil, octCPS, ramp_, ratioMIDI, softClip, squared
         ]
  fmap fun (elements =<< sequence [list_patterns, random_patterns])

fractional_patterns = do
  fun <- elements [ \x -> x / x, recip ]
  fmap fun (elements =<< sequence [list_patterns, random_patterns])

floating_patterns = do
  fun <- elements
    [ const pi, exp, sqrt, log, \x -> x ** x, \x -> logBase x x
    , sin, cos, tan, asin, atan, acos, sinh, tanh, cosh, asinh, atanh, acosh ]
  fmap fun (elements =<< sequence [list_patterns, random_patterns])

make_patterns f = elements =<<
  (f <$> (elements (non_empty_prims (1,[1,2,3],4))) <*>
   (elements (non_empty_prims (10,[10,20,30],5))) <*>
   (elements (non_empty_prims (10,[10,20,30],5))))
