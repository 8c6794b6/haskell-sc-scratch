{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (ScopedTypeVariables)

Tests for 'R' interpreter.

-}
module Test.Sound.SC3.Lepton.Pattern.Interpreter.R where

import Control.Applicative
import Data.Data
import Data.Ratio ((%))
import Data.Word (Word64)
import Test.QuickCheck

import Control.DeepSeq
import Sound.SC3
import System.Random.Mersenne.Pure64

import Sound.SC3.Lepton.Pattern.Expression
-- import Sound.SC3.Lepton.Pattern.Interpreter.R
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Data.Map as M
import qualified Test.QuickCheck.Monadic as QM

tests = []
-- tests =
--   [ label "prop_prims" prop_prims
--   , label "prop_numeric" prop_numeric
--   , label "prop_fromx" prop_fromx
--   , label "prop_io" prop_io
--   , label "prop_pappend" prop_pappend
--   , label "prop_pseq" prop_pseq
--   , label "prop_preplicate" prop_preplicate
--   , label "prop_prand_pchoose" prop_prand_pchoose
--   , label "prop_prange" prop_prange
--   , label "prop_prandom" prop_prandom
--   , label "prop_pcycle" prop_pcycle
--   , label "prop_pshuffle" prop_pshuffle
--   , label "prop_psnew" prop_psnew
--   , label "prop_pnset" prop_pnset
--   , label "prop_ppar" prop_ppar
--   ]

-- prop_prims = forAll arbitrary $ \(x::Double,xs::[Double],y::Double) ->
--   rp 0 (pval x) == [x] &&
--   rp 0 pempty == ([] :: [Double]) &&
--   rp 0 (plist xs) == xs &&
--   and (zipWith (==) (take 100 (rp 0 (prepeat y))) (repeat y))

-- prop_numeric =
--   forAll (arbitrary `suchThat` (\(x::Double, xs::[Double], y::Double) ->
--            and [x /= 0, all (/= 0) xs, y /= 0])) $ \(x,xs,y) ->
--   let zl2 op = make_ziplist op y xs
--       rs2 op = rp 0 (plist xs `op` prepeat y)
--       zl1 f = map f (y:xs)
--       rs1 f = rp 0 (f (pconcat [pval y, plist xs]))
--       no_eq x = isNaN x || isInfinite x
--   in and [ -- Num
--            zl2 (+) == rs2 (+)
--          , zl2 (*) == rs2 (*)
--          , zl2 (-) == rs2 (-)
--          , zl1 negate == rs1 negate
--          , zl1 abs == rs1 abs
--          , zl1 signum == rs1 signum

--            -- Fractional
--          , zl2 (/) == rs2 (/)
--          , zl1 recip == rs1 recip

--            -- Floating
--          , zl1 exp == rs1 exp
--          , zl1 sqrt == rs1 sqrt || any no_eq (rs1 sqrt)
--          , zl1 log == rs1 log || any no_eq (rs1 log)
--          , zl2 (**) == rs2 (**) || any no_eq (rs2 (**))
--          , zl2 logBase == rs2 logBase || any no_eq (rs2 logBase)
--          , zl1 sin == rs1 sin
--          , zl1 tan == rs1 tan || any no_eq (rs1 tan)
--          , zl1 cos == rs1 cos
--          , zl1 asin == rs1 asin || any no_eq (rs1 asin)
--          , zl1 atan == rs1 atan || any no_eq (rs1 atan)
--          , zl1 acos == rs1 acos || any no_eq (rs1 acos)
--          , zl1 sinh == rs1 sinh || any no_eq (rs1 sinh)
--          , zl1 tanh == rs1 tanh || any no_eq (rs1 tanh)
--          , zl1 cosh == rs1 cosh || any no_eq (rs1 cosh)
--          , zl1 asinh == rs1 asinh || any no_eq (rs1 asinh)
--          , zl1 atanh == rs1 atanh || any no_eq (rs1 atanh)
--          , zl1 acosh == rs1 acosh || any no_eq (rs1 acosh)

--            -- Unary
--          , zl1 ampDb == rs1 ampDb || any no_eq (rs1 ampDb)
--          , zl1 cpsMIDI == rs1 cpsMIDI || any no_eq (rs1 cpsMIDI)
--          , zl1 cpsOct == rs1 cpsOct || any no_eq (rs1 cpsOct)
--          , zl1 log10 == rs1 log10 || any no_eq (rs1 log10)
--          , zl1 log2 == rs1 log2 || any no_eq (rs1 log2)
--          , zl1 midiCPS == rs1 midiCPS || any no_eq (rs1 midiCPS)
--          , zl1 midiRatio == rs1 midiRatio || any no_eq (rs1 midiRatio)
--          , zl1 notE == rs1 notE || any no_eq (rs1 notE)
--          , zl1 notNil == rs1 notNil || any no_eq (rs1 notNil)
--          , zl1 octCPS == rs1 octCPS || any no_eq (rs1 octCPS)
--          , zl1 ratioMIDI == rs1 ratioMIDI || any no_eq (rs1 ratioMIDI)
--          , zl1 squared == rs1 squared || any no_eq (rs1 squared)
--          ]

-- prop_fromx = forAll (arbitrary `suchThat` (/= 0)) $ \(a :: Integer) ->
--   forAll arbitrary $ \(b::Integer,c::Int) ->
--   let r = pconcat [fromIntegral a, fromRational (b%a), toEnum c, pi]
--       rs = rp 0 r
--   in  (rs::[Double]) `deepseq` not (null rs) &&
--       typeOf (toR pi :: R Double) == typeOf (undefined :: R Double)

-- prop_io = mapSize (min 7) $ forAll arbitrary $ \(Positive x,y,ys) ->
--   QM.monadicIO $ do
--     let r = pseq (pval x) [pval y, plist ys]
--     rs1 <- QM.run $ runPIO r
--     QM.run $ foldPIO_ (\b a -> return (a:b)) [] r
--     QM.run $ mapPIO_ return r
--     QM.assert $ not (null (rs1::[Double]))

-- prop_pappend =
--   forAll ((,) <$> arbitrary <*> arbitrary) $ \(x,xs) ->
--   rp 0 (pappend (pval x) (plist xs)) == (x:xs :: [Double])

-- prop_pseq = mapSize (min 10) $
--   forAll arbitrary $ \(Positive i) ->
--   forAll ((,) <$> arbitrary <*> arbitrary) $ \(xs::[Double],ys::[Double]) ->
--   let rs = rp 0 $ pseq (pval i) ((map pval xs) ++ [plist ys])
--       zs = concat $ replicate i (xs++ys)
--   in  rs == zs

-- prop_preplicate = mapSize (min 10) $
--   forAll arbitrary $ \(Positive i) ->
--   forAll arbitrary $ \(x::Double) ->
--   let rs = rp 0 $ preplicate (pval i) (pval x)
--       zs = replicate i x
--   in  rs == zs

-- prop_prand_pchoose = mapSize (min 10) $
--   forAll arbitrary $ \(Positive i, seed) ->
--   forAll arbitrary $ \(xs::[Double]) ->
--   not (null xs) ==>
--   let rs = rp seed $ prand (pval i) (map pval xs)
--       rs' = rp seed $ pchoose (pval i) (map pval xs)
--   in  rs `deepseq` not (null rs) &&
--       rs' `deepseq` not (null rs')

-- prop_prange =
--   forAll arbitrary $ \seed ->
--   forAll (arbitrary `suchThat` \(lo::Double,hi::Double) -> lo < hi) $
--   \(lo,hi) ->
--   let rs = take 200 $ rp seed $ pforever (prange (pval lo) (pval hi))
--   in  lo <= minimum rs && maximum rs <= hi

-- prop_prandom = forAll arbitrary $ \seed ->
--   let rs = take 200 $ rp seed (pforever prandom) :: [Double]
--   in  rs `deepseq` not (null rs)

-- prop_pcycle = forAll arbitrary $ \(seed,x,y,z) ->
--   let rs = take 100 $ rp seed (pcycle [pval x, pval y, pval z])
--   in  rs == (take 100 $ cycle [x,y,z] :: [Double])

-- prop_pshuffle = forAll arbitrary $ \(seed,x,y,z) ->
--   let rs = rp seed $ pshuffle [pval x, pval y, pval z]
--   in  sum [x,y,z] == (sum rs :: Integer)

-- prop_psnew = forAll arbitrary $ \(x,y) ->
--   let rs = rp 0 $
--            psnew "foo" Nothing AddToTail 1 [("bar", pval x),("buzz", pval y)]
--       zs = ToOSC (Snew "foo" Nothing AddToTail 1)
--            (M.fromList [("bar",x), ("buzz",y)])
--   in  head rs == zs

-- prop_pnset = forAll arbitrary $ \(x,y,z) ->
--   let rs = rp 0 $
--            pnset z [("bar", pval x),("buzz", pval y)]
--       zs = ToOSC (Nset z) (M.fromList [("bar",x), ("buzz",y)])
--   in  head rs == zs

-- prop_ppar = forAll arbitrary $ \(x1,x2,Positive t1,Positive t2) ->
--   let r1 = psnew "foo" Nothing AddToTail 1 [("dur",plist [t1,t1]),("x",prepeat x1)]
--       r2 = psnew "foo" Nothing AddToTail 1 [("dur",plist [t2,t2]),("x",prepeat x2)]
--       rs = rp 0 $ ppar [r1,r2]
--       mko t x = ToOSC (Snew "foo" Nothing AddToTail 1)
--               (M.fromList [("dur",t),("x",x)])
--       o1 = mko t1 x1; o2 = mko t2 x2;
--       r0 = head $ drop 2 rs
--   in  (t1 < t2 ==> r0 == o1) .||. (t2 > t1 ==> r0 == o2)

-- make_ziplist op x xs = getZipList (op <$> ZipList xs <*> pure x)

-- ------------------------------------------------------------------------------
-- -- Util

-- rp :: Word64 -> R a -> [a]
-- rp seed pat = runP (toR pat) (pureMT seed)
