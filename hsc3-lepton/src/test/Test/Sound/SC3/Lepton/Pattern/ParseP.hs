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
module Test.Sound.SC3.Lepton.Pattern.ParseP where

import Test.QuickCheck

import Sound.SC3
import System.Random.Mersenne.Pure64

-- import Sound.SC3.Lepton.Pattern.Dummy
-- import Sound.SC3.Lepton.Pattern.Expression
-- import Sound.SC3.Lepton.Pattern.ParseP
-- import Sound.SC3.Lepton.Pattern.Interpreter.Bz
-- import Sound.SC3.Lepton.Pattern.Interpreter.R

import qualified Data.Attoparsec.Lazy as A
import qualified Data.Attoparsec.Char8 as A hiding (Result(..),parse)
import qualified Data.ByteString.Lazy as BSL

import Test.Sound.SC3.Lepton.Pattern.Interpreter hiding (tests)

tests :: [Property]
tests = []
-- tests =
--   [ label "parse_nums" prop_nums
--   , label "parse_snew_nset" prop_snew_nset
--   , label "parse_run" prop_run
--   ]

-- prop_nums =
--   forAll (arbitrary `suchThat` (\(a,b) -> abs a < 500 && abs b < 500))
--   $ \(a::Double,b::Double) ->
--   let bz = mkNumVal a b
--       A.Done _ r = A.parse dPatterns (lazyByteStringP bz)
--   in  not (BSL.null (lazyByteStringP $ toBz r))

-- prop_snew_nset = forAll mixed_patterns $ \p ->
--   let p' = mkOscP p
--       Right r = parseP $ lazyByteStringP p'
--   in  not (BSL.null (lazyByteStringP $ toBz r))

-- prop_run =
--   mapSize (min 20) $ forAll arbitrary $ \(Positive x) ->
--   let p = pseq (pval x) [pval 1, plist [2,3,4]] :: Bz Double
--       A.Done _ r = A.parse dPatterns $ lazyByteStringP p
--   in  runP r (pureMT (fromIntegral x)) == concat (replicate x [1,2,3,4])

-- mkNumVal a b  =
--   ((pval a + pval b) * (pval a * pval b) +
--    (pval a - pval b) + abs (pval a) * negate (pval b) + signum (pval b) +
--    (pval a / pval b) + (pval pi ** pval (exp a)) + (log (pval a)) +
--    (sqrt (pval (b**2))) +
--    (sin (pval b)) + cos (pval a) + tan (pval (a*pi)) +
--    asin (pval (a**2)) + atan (pval (b**2)) + acos (pval (a**2)) +
--    sinh (pval a) + tanh (pval a) + cosh (pval b) +
--    asinh (pval a) + atanh (pval b) + acosh (pval b))

-- mkOscP p = ppar [snew_foo p, nset_100 p {- , pmerge (snew_foo p) (nset_100 p)-} ]

-- -- This showed couple failing results.
-- --
-- -- prop_snew_nset = forAll num_patterns $ \p ->
-- --   let p' = pconcat [ ppar [snew_foo p, nset_100 p]
-- --                    , pmerge (snew_foo p) (nset_100 p) ]
-- --       Right r = parseP $ lazyByteStringP p'
-- --   in  not (BSL.null (lazyByteStringP $ toBz r))


-- -- mkOscP' p = pconcat [ ppar [snew_foo p, nset_100 p]
-- --                    , pmerge (snew_foo p) (nset_100 p) ]
-- -- mkOscP' (c1+c1) passes, mkOscP (c1*c1) fails.
-- --
-- -- c1 = (prand (plist [2075504499,-2646233896,-1655047235,-985753979,2422595745,-3339781871,2849530678,1477266023,-481462658,-3226623338,-3669844831,-3830800733,2222813396,-1552240009,2780114320,2854385260,-1421059887,628791748,-1331225559,1697652844,461253265,3911746257,2849043643,-3447167784,1428484054,3701899775,2409991673,2935518606,-1770063212,1613698408,2347588864,653276355,2337395795,-3715918946,-2043391350,-2597743004,4265214914,518008669,1034250707,1690514405,-3541798257,-2517903289,-2971451976,-150047114,-3487123913,599336011]) [pval 112.80815227069732,pempty])

-- -- mkOscP' (c2+c2) passes, mkOscP (c2*c2) fails.
-- --
-- -- c2 = (pshuffle [plist [3.4471549477259917,-1061.7163946581431,-268.1840853748786,-73.24786045589568,26.32019163230154,-12.90430584515738,-131.3839898411476,29.31673658290952,18.282872518566908,197.4502760683972,-31.76847612504456,25.185667679108786,19.893894768193693,-11.19385988825506,-50.05248909330462,1966.0204139668379,-394.8482511289382,69.81660532569742,29.182132839354107,-160.52069873724932,-94.28119889717537,64.13722404377566,-59.77492016197106,22.0571774982998,24.014070456112787,51.92147469106574,-19.611712904259438,144.0441810806422],pval 23.016599856091403])
