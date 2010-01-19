------------------------------------------------------------------------------
-- | Taking a tour of ugens again.
-- 

module SCHelp.TourOfUGens where

import Sound.SC3
import Sound.SC3.UGen.Dot
import Sound.OpenSoundControl

import Reusable
import Instances
import SCTree
import SCQuery
import SCSched

r :: IO ()
r = withSC3 reset

-- 
-- Periodic Sources: Oscillators
--

-- 
-- LF - "Low Frequency" Unit Generators
-- 

type RateAnd2In = Rate -> UGen -> UGen -> UGen

-- | Try lfEx[01,02,03,04] with lfPar, lfCub, lfTri, lfSaw. 
--
-- e.g.
-- 
-- > > lfEx01 lfCub
-- > > lfEx01 lfPar
-- > > lfEx02 lfPar
-- 
lfEx01 :: RateAnd2In -> UGen
lfEx01 lf = out 0 $ 
            lf ar (lf kr (lf kr 0.2 0.8 * 10) 0 * 400 + 800) 0 * 0.1

lfEx02 :: RateAnd2In -> UGen
lfEx02 lf = out 0 $ lf ar (lf kr 0.2 0 * 400 + 800) 0 * 0.1

lfEx03 :: RateAnd2In -> UGen
lfEx03 lf = out 0 $ lf ar 800 0 * 0.1

lfEx04 :: RateAnd2In -> UGen
lfEx04 lf = out 0 $ lf ar (xLine kr 100 15000 6 DoNothing) 0 * 0.1

type RateAnd3In = Rate -> UGen -> UGen -> UGen -> UGen

-- | Try lfEx[05,06,07] with lfPulse and varSaw.
-- Or, some of them might be interesting like:
--
-- > > lfEx05 (\r a b _ -> sinOsc r a b)
-- 
lfEx05 :: RateAnd3In -> UGen
lfEx05 lf = out 0 $ 
            lf ar (lf kr (lf kr 0.2 0 0.5 * 8 + 10) 0 0.5 * 400 +800) 0 0.5 * 0.1

lfEx06 :: RateAnd3In -> UGen
lfEx06 lf = out 0 $ lf ar (lf kr 3 0 0.3 * 200 + 200) 0 0.2 * 0.1

lfEx07 :: RateAnd3In -> UGen
lfEx07 lf = out 0 $ lf ar (xLine kr 100 15000 6 DoNothing) 0 0.5 * 0.1

lfEx08 :: RateAnd3In -> UGen
lfEx08 lf = out 0 $ lf ar 100 0 (mouseY kr 0 1 Linear 0.1) * 0.1 

lfEx09 :: RateAnd3In -> UGen
lfEx09 lf = out 0 $ lf ar 100 0 (lfTri kr 0.2 0 * 0.5 + 0.5) * 0.1

blipEx01 :: UGen 
blipEx01 = out 0 $ blip ar (xLine kr 20000 200 6 DoNothing) 100 * 0.2

blipEx02 :: UGen
blipEx02 = out 0 $ blip ar (xLine kr 100 15000 6 DoNothing) 100 * 0.2

-- | Try:
-- 
-- > > blipEx03 (line kr 1 100 20 DoNothing)
-- > > blipEx03 (mouseX kr 1 100 Linear 0.1)
-- 
blipEx03 :: UGen -> UGen
blipEx03 numHarm = out 0 $ blip ar 200 numHarm * 0.2


