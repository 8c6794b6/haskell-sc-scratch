{-# LANGUAGE Rank2Types #-}
------------------------------------------------------------------------------
-- | Taking a tour of ugens again.
--

module SCHelp.TourOfUGens where

import Control.Applicative
import Control.Monad
import System.Random

import Sound.SC3
import Sound.SC3.UGen.Dot
import Sound.SC3.UGen.UGen
import Sound.OpenSoundControl

import Reusable
import Instances
import Missing
import SCTree
import SCQuery
import SCSched
import qualified Scratch.ControlArgs as Arg


r :: IO ()
r = withSC3 reset

d :: IO ()
d = withSC3 queryAllNodes

--
-- Periodic Sources: Oscillators
--

--
-- LF - "Low Frequency" Unit Generators
--

type RateAnd1In = Rate -> UGen -> UGen
type RateAnd2In = Rate -> UGen -> UGen -> UGen
type RateAnd3In = Rate -> UGen -> UGen -> UGen -> UGen

-- | Type synonym for lfNoise0, lfNoise1, lfNoise2, lfClipNoise, dust, and dust2
type NoiseM1 = (UId m) => Rate -> UGen -> m UGen

-- | Type synonym for clipNoise, whiteNoise, pinkNoise, brownNoise,
-- and grayNoise.
type NoiseM0 = (UId m) => Rate -> m UGen

-- | Type synonym for lpf, hpf (in, freq).
type Filter1 = UGen -> UGen -> UGen

-- | Type synonym for:
--
-- * bpf, brf, rlpf, rhpf, resonz (in, freq, rq)
-- * ringz (in, freq, ring time)
--
type Filter2 = UGen -> UGen -> UGen -> UGen

-- | Type synonym for formlet.
type Filter4 = UGen -> UGen -> UGen -> UGen -> UGen

-- | For klank ugen, just for readability.
type KlankSpec = UGen

-- | For delayN, delayL, and delayC.
type Delay3 = UGen -> UGen -> UGen -> UGen

-- | for comb[N,L,C] and allpass[N,L,C]
type Delay4 = UGen -> UGen -> UGen -> UGen -> UGen

-- | for decay
type Control2 = UGen -> UGen -> UGen

-- | for decay2
type Control3 = UGen -> UGen -> UGen -> UGen

-- | for trig, trig1, tDelay, latch, gate, pulseCount
type Trig2 = UGen -> UGen -> UGen

-- | for pulseDivider
type Trig3 = UGen -> UGen -> UGen -> UGen

-- | PV ugen with 2 inputs.
type PV2 = UGen -> UGen -> UGen

-- | For PV_RandComb.
type PV3M = UId m => UGen -> UGen -> UGen -> m UGen

-- | For PV_RectComb.
type PV4 = UGen -> UGen -> UGen -> UGen -> UGen

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

lfEx045 :: RateAnd2In -> UGen
lfEx045 lf = out 0 $ lf ar (xLine kr 20000 200 6 DoNothing) 0 * 0.1



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

oneInEx01 :: RateAnd1In -> UGen
oneInEx01 ug = out 0 $ ug ar (xLine kr 20000 200 6 DoNothing) * 0.2

oneInEx02 :: RateAnd1In -> UGen
oneInEx02 ug = out 0 $ ug ar (xLine kr 100 15000 6 DoNothing) * 0.2

-- | Second arg for pulse is width. If it's 0, no sound.
pulseEx01 :: UGen
pulseEx01 = out 0 $ pulse ar (xLine kr 20000 200 6 DoNothing) 0.3 * 0.2

pulseEx02 :: UGen
pulseEx02 = out 0 $ pulse ar (xLine kr 100 15000 6 DoNothing) 0.3 * 0.2

pulseEx03 :: UGen
pulseEx03 = out 0 $ pulse ar 200 (line kr 0.01 0.99 8 DoNothing)  * 0.2

pulseEx04 :: UGen
pulseEx04 = out 0 $ rlpf (pulse ar (mce [100,250]) 0.5 * 0.1)
            (xLine kr 8000 400 5 DoNothing) 0.05

pulseEx05 :: IO UGen
pulseEx05 = do
  n <- pure ((* 0.5) . (+ 0.5)) <*> (clone 3 (lfNoise2 kr 1))
  return $ out 0 $ mix $ pulse ar (mce [120,180.42,239.81]) n * 0.1

-- | From hsc3 help file.
klangEx01 :: UGen
klangEx01 = out 0 $ klang ar 1 0 $ klangSpec f a p
    where
      f = [440,550..1100]
      a = take 7 $ cycle [0.05,0.02]
      p = replicate 7 0

-- | From tour of ugen
klangEx02 :: UGen
klangEx02 = out 0 $ klang ar 1 0 (klangSpec f a p) * 0.2
    where
      f = [800,1000,1200]
      a = [0.3,0.3,0.3]
      p = [pi,pi,pi]

klangEx03 :: IO UGen
klangEx03 = do
  gen <- newStdGen
  let fs = take 16 $ fmap exp $ randomRs (log 400, log 2000) gen
      as = take 16 $ repeat 0.1
      ps = take 16 $ repeat 0
  return $ out 0 $ klang ar 1 0 (klangSpec fs as ps) * 0.2

klangEx04 :: IO UGen
klangEx04 = do
  gen <- newStdGen
  let fs = take 16 $ fmap exp $ randomRs (log 400, log 2000) gen
      as = take 16 $ repeat 0.1
      ps = take 16 $ repeat 0
      fscale = mouseX kr 0.5 10 Exponential 0.1
      foffset = mouseY kr 0 1000 Linear 0.1
  return $ out 0 $ klang ar fscale foffset (klangSpec fs as ps) * 0.2

oscBufNum :: Num a => a
oscBufNum = 1

allocateOscBuffer :: IO ()
allocateOscBuffer = flip query s $ do
  msg $ b_free oscBufNum
  msg $ b_alloc oscBufNum 512 1
  msg $ b_gen oscBufNum "sine1" [1 + 2 + 4, 1, 0.5,0.25,0.125]

-- | Fill wavetable buffer used in oscEx01.
fillOscBuf :: [Double] -> IO ()
fillOscBuf ps = query (msg $ b_gen oscBufNum "sine1" (7:ps)) s

oscEx01 :: UGen
oscEx01 = out 0 $ osc ar oscBufNum 100 0 * 0.1

vOscBufNums :: (Enum a, Num a) => [a]
vOscBufNums = [80..87]

allocateVoscBuffers :: IO ()
allocateVoscBuffers = flip query s $
  do mapM_ (\n -> msg $ b_free n) vOscBufNums
     mapM_ (\n -> msg $ b_alloc n 1024 1) vOscBufNums

fillVOscBufs :: IO ()
fillVOscBufs = mapM_ f vOscBufNums
  where f n = query (msg $ b_gen n "sine1" (gen n)) s
        gen n = 7:(map (fromIntegral . (\x -> x * x)) [1..n-79])

vOscEx01 :: UGen
vOscEx01 = out 0 $ vOsc ar x (mce [100,101]) 0 * 0.1
    where x = mouseX kr 80 87 Linear 0.1

vOscEx02 :: UGen
vOscEx02 = out 0 $ vOsc ar idx (mce [100,101]) 0 * 0.1
    where idx = (sinOsc ar 0.2 0 * 3.49 + 3.49) + 80

vOscEx03 :: UGen -> UGen
vOscEx03 freq = vOsc ar idx freq 0 * 0.1
    where idx = mouseX kr 80 87 Linear 0.1

vOscEx04 :: UGen -> IO UGen
vOscEx04 freq = do
  idx <- lfNoise2 kr 2 >>. (*3.49) >>. (+ 3.49)
  return $ vOsc ar idx freq 0 * 0.1

vOsc3Ex01 :: UGen
vOsc3Ex01 = out 0 $ vOsc3 ar idx 120 121.04 119.37 * 0.2
    where idx = mouseX kr 80 87 Linear 0.1

vOsc3Ex02 :: UGen
vOsc3Ex02 = out 0 $ vOsc3 ar idx 120 151.13 179.42 * 0.2
    where idx = mouseX kr 80 87 Linear 0.1

lfNEx01 :: NoiseM1 -> IO UGen
lfNEx01 ug = ug ar (mouseX kr 200 10000 Linear 1) >>. (* 0.125) >>. out 0

lfNEx02 :: NoiseM1 -> IO UGen
lfNEx02 ug = do
  n <- ((*200) . (+400)) <$> ug kr (mouseX kr 0.5 64 Linear 1)
  return $ out 0 $ lfPar ar n 0 * 0.2

nzEx01 :: NoiseM0 -> IO UGen
nzEx01 ug = (out 0 . (* 0.2)) <$> ug ar

-- | Try dust, dust2.
dustEx01 :: NoiseM1 -> IO UGen
dustEx01 ug = do
  let idx = mouseX kr 1 10000 Exponential 1
  pure (out 0 . (* 0.4)) <*> (ug ar idx)

-- | Theres more chaotic noise ugens like henonN, lorenzL, etc.
crkEx01 :: UGen
crkEx01 = out 0 $ crackle ar (mouseX kr 1 2 Linear 0.1) * 0.5

--
-- Filters
--

f1Ex01 :: Filter1 -> IO UGen
f1Ex01 ug = out 0 . (* 0.2) . flip ug (mouseX kr 1e2 2e4 Exponential 0.2)
                <$> whiteNoise ar

f1Ex02 :: Filter1 -> UGen
f1Ex02 ug = out 0 $
                ug (saw ar 100) (mouseX kr 1e2 2e4 Exponential 0.1) * 0.2

f2Ex01 :: Filter2 -> IO UGen
f2Ex01 ug = do
  n <- whiteNoise ar
  return $ out 0 $ ug n (mouseX kr 1e2 2e4 Exponential 0.1)
            (mouseY kr 0.001 0.999 Linear 0.2) * 0.2

f2Ex02 :: Filter2 -> UGen
f2Ex02 ug = out 0 $
            ug (saw ar 100) (mouseX kr 1e2 2e4 Exponential 0.1)
                   (mouseY kr 0.001 0.99 Linear 0.2) * 0.2

f2Ex03 :: Filter2 -> IO UGen
f2Ex03 ug = do
  n <- whiteNoise ar
  return $ out 0 $ ug n (xLine kr 1000 8000 10 DoNothing) 0.05 * 0.1

f2Ex04 :: Filter2 -> IO UGen
f2Ex04 ug = do
  n <- whiteNoise ar
  return $ out 0 $ ug n 2000 (xLine kr 1 0.001 8 DoNothing) * 0.1

f2Ex05 :: Filter2 -> IO UGen
f2Ex05 ug = do
  n <- whiteNoise ar
  return $ out 0 $ ug n 2000 (xLine kr 0.001 1 8 DoNothing) * 0.1

f2Ex06 :: Filter2 -> IO UGen
f2Ex06 ug = do
  n <- (* 0.3) <$> dust ar 3
  return $ out 0 $ ug n 2000 2

f2Ex07 :: Filter2 -> IO UGen
f2Ex07 ug = do
  n <- (* 0.005) <$> whiteNoise ar
  return $ out 0 $ ug n 2000 0.5

f2Ex08 :: Filter2 -> IO UGen
f2Ex08 ug = do
  n <- (* 0.005) <$> whiteNoise ar
  return $ out 0 $ ug n (xLine kr 100 3000 10 DoNothing) 0.5

f2Ex09 :: Filter2 -> UGen
f2Ex09 ug = out 0 $ ug i (xLine kr 100 3000 10 DoNothing) 0.5
    where i = impulse ar 6 0 * 0.3

f2Ex10 :: Filter2 -> UGen
f2Ex10 ug = out 0 $ ug i 2000 (xLine kr 0.04 4 8 DoNothing)
    where i = impulse ar 6 0 * 0.3

f2Ex11 :: Filter2 -> IO UGen
f2Ex11 ug = do
  rt <- ((*1.9) . (+ 1.94)) <$> lfNoise1 ar 1
  nz <- (* 0.3) <$> dust ar 3
  return $ out 0 $ ug nz 2000 rt

-- | Example for onePole, oneZero.
oneFEx01 :: Filter1 -> IO UGen
oneFEx01 ug = do
  n <- (* 0.5) <$> whiteNoise ar
  return $ out 0 $ ug n $ mouseX kr (-0.99) 0.99 Linear 0.1

oneFEx02 :: Filter1 -> IO UGen
oneFEx02 ug = do
  n <- (* 0.5) <$> whiteNoise ar
  return $ out 0 $ ug n $ mouseX kr (-0.49) 0.49 Linear 0.1

medianEx01 :: Filter1 -> IO UGen
medianEx01 ug = do
  n <- (* 0.9) <$> dust2 ar 100
  return $ out 0 $ ug 3 (saw ar 500 * 0.1 + n)

slewEx01 :: Filter2 -> IO UGen
slewEx01 ug = do
  n <- (* 0.9) <$> dust2 ar 100
  return $ out 0 $ ug (saw ar 500 * 0.1 + n) 1000 1000

formletEx01 :: Filter4 -> UGen
formletEx01 ug = out 0 $ ug i 800 0.01 0.1
    where i = impulse ar (mouseX kr 2 300 Linear 1) 0 * 0.4

formletEx02 :: Filter4 -> IO UGen
formletEx02 ug = do
  n <- dust2 ar (mouseX kr 2 300 Linear 1) >>. (* 0.4)
  return $ out 0 $ ug n 800 0.01 0.1

klankEx01 :: UGen
klankEx01 = out 0 $ klank source 1 0 1 spec
    where
      source = impulse ar 2 0 * 0.1
      spec = klankSpec
                 [200, 671, 1153, 1723]
                 [1,1,1,1]
                 [1,1,1,1]

klankEx02 :: IO UGen -> IO UGen
klankEx02 nz = fmap (\n -> out 0 $ klank n 1 0 1 spec) nz
    where
      spec = klankSpec
             [200, 671, 1153, 1732]
             [1,1,1,1]
             [1,1,1,1]

klankEx03 :: KlankSpec -> IO UGen -> IO UGen
klankEx03 spec nz = fmap (\n -> out 0 $ klank n 1 0 1 spec) nz

kkSpec01 :: IO KlankSpec
kkSpec01 = do
  gen <- newStdGen
  return $ klankSpec
           (take 12 $ map exp $ randomRs (log 200, log 4000) gen)
           (take 12 $ repeat 1)
           (take 12 $ repeat 1)

kkSpec02 :: KlankSpec
kkSpec02 = klankSpec
           (map (*200) [1..13])
           (map recip [1..13])
           (replicate 13 1)

kkSpec03 :: KlankSpec
kkSpec03 = klankSpec
           (map (*200) [1,3..13])
           (map recip [1,3..13])
           (replicate 7 1)

kkSpec04 :: IO KlankSpec
kkSpec04 = do
  gen <- newStdGen
  let n = 128
      fs = take n $ map exp $ randomRs (log 100,log 8000) gen
      as = take n $ randomRs (0.1,1.0) gen
      ds = take n $ randomRs (0.001,1.0) gen
  return $ klankSpec fs as ds

--
-- Distortion
--

unaryOpEx01 :: (UGen -> UGen) -> UGen
unaryOpEx01 uo = out 0 $ uo $ sinOsc ar 300 0 * 0.2

binaryOpEx01 :: (UGen -> UGen -> UGen) -> UGen
binaryOpEx01 bo = out 0 $ bo (sinOsc ar 300 0 * amp) 1 * 0.2
    where amp = mouseX kr 0.1 80 Linear 1

zeroHzSinEx01 :: UGen
zeroHzSinEx01 = out 0 $ sinOsc ar 0 i * 0.2
    where i = sinOsc ar 300 0 * mouseX kr 0.1 (8*pi) Linear 1

shaperBufNum :: Num a => a
shaperBufNum = 90

drawShaperBuf :: IO ()
drawShaperBuf = withSC3 $ \fd -> do
                  send fd $ b_alloc shaperBufNum 1024 1
                  send fd $ b_gen shaperBufNum "cheby" [0,1,0,1,1,0,1]

shaperEx01 :: UGen
shaperEx01 = out 0 $ shaper shaperBufNum idx * 0.3
    where idx = sinOsc ar 600 0 * mouseX kr 0 1 Linear 0.1

--
-- Panner
--

panEx01 :: IO UGen
panEx01 = do
  n <- brownNoise ar
  return $ out 0 $ pan2 n (mouseX kr (-1) 1 Linear 0.1) 0.3

panEx02 :: UGen -> IO UGen
panEx02 ug = do
  n <- brownNoise ar
  return $ out 0 $ pan2 n ug 0.3

linPanEx01 :: UGen -> IO UGen
linPanEx01 ug = fmap (\n -> out 0 $ linPan2 n ug 0.3) $ brownNoise ar

balanceEx01 :: IO UGen
balanceEx01 = (\n n' -> out 0 $ balance2 n n' pos 0.3) <$> brownNoise ar <*> brownNoise ar
    where pos = mouseX kr (-1) 1 Linear 0.1

xfade2Ex01 :: IO UGen
xfade2Ex01 = do
  n <- brownNoise ar
  let s = sinOsc ar 500 0
  return $ out 0 $ xFade2 n s (mouseX kr (-1) 1 Linear 0.1) 0.3

-- | .. how do i use this?
panB2Ex01 :: IO UGen
panB2Ex01 = do
  n <- brownNoise ar
  return $ out 0 $ panB2 n (mouseX kr (-1) 1 Linear 0.1) 0.3

rotate2Ex01 :: IO UGen
rotate2Ex01 = do
  x <- (* 0.3) <$> brownNoise ar
  let y = saw ar 200 * 0.3
  return $ out 0 $ rotate2 x y (lfSaw kr 0.1 0)

--
-- Reverbs, cant find gverb ugen.
--

freeVerbEx01 :: IO UGen
freeVerbEx01 = do
  d <- dust ar 2 >>. (* 0.1)
  spec <- kkSpec04
  let x' = klank d 1 0 1 spec
      x = mce [x', delayC x' 0.01 0.01]
  return $ out 0 $ freeVerb x 0.75 0.9 0.4

--
-- Delays and Buffer ugens
--

delayEx01 :: Delay3 -> IO UGen
delayEx01 ug = do
  z <- (\t n -> decay (t*0.5) 0.3 * n) <$> dust ar 1 <*> whiteNoise ar
  return $ out 0 $ ug z 0.1 0.1 + z

delayEx02 :: Delay3 -> IO UGen
delayEx02 ug = do
  z <- (\n -> decay (impulse ar 2 0 *0.4) 0.3 * n) <$> whiteNoise ar
  return $ out 0 $ ug z 0.3 (mouseX kr 0 0.3 Linear 0.1) + z

fbEx01 :: Delay4 -> IO UGen
fbEx01 ug = do
  z <- (\t n -> decay (t*0.5) 0.2 * n) <$> dust ar 1 <*> whiteNoise ar
  return $ out 0 $ ug z 0.2 0.2 3

fbEx02 :: Delay4 -> IO UGen
fbEx02 ug =
  (\n -> out 0 $ ug (n*0.02) 0.01 (xLine kr 0.0001 0.01 20 DoNothing) 0.2)
  <$> whiteNoise ar

fbEx03 :: Delay4 -> IO UGen
fbEx03 ug =
  (\n -> out 0 $ ug (n*0.02) 0.01 (xLine kr 0.0001 0.01 20 DoNothing) (-0.2))
  <$> whiteNoise ar

apEx01 :: Delay4 -> IO UGen
apEx01 ug = do
  z <- (\t n -> decay (t * 0.5) 0.1 * n) <$> dust ar 1 <*> whiteNoise ar
  let g a b = (\dt -> ug a 0.04 dt 2) <$> randomRIO (0,0.04)
  out 0 <$> foldM g z [1..8]

playBufNum :: Num a => a
playBufNum = 100

allocReadPlayBuf :: FilePath -> IO OSC
allocReadPlayBuf sf =
    withSC3 $ \fd -> do
      async fd $ b_free playBufNum
      async fd $ b_allocRead playBufNum sf 0 0

-- | palyBuf numChannels bufnum rate trig startpos loop doneAction
pbEx01 :: UGen
pbEx01 = out 0 $ sinOsc ar (800 + 700 * pb) 0 * 0.3
    where
      pb = playBuf 1 playBufNum (bufRateScale kr playBufNum)
           1 0 Loop DoNothing

pbEx02 :: UGen
pbEx02 = out 0 $ playBuf 1 playBufNum (bufRateScale kr playBufNum)
         1 0 Loop DoNothing

pbEx03 :: UGen
pbEx03 =
    out 0 $ playBuf 1 playBufNum (bufRateScale kr playBufNum)
    t 0 NoLoop DoNothing
    where
      t = impulse kr 2 0

pbEx04 :: UGen
pbEx04 =
    out 0 $ playBuf 1 playBufNum (bufRateScale kr playBufNum)
    t 0 NoLoop DoNothing
    where
      t = impulse kr (xLine kr 0.1 100 30 RemoveSynth) 0

pbEx05 :: UGen
pbEx05 =
    out 0 $ playBuf 1 playBufNum (bufRateScale kr playBufNum)
    t p Loop DoNothing
    where
      t = impulse kr (mouseY kr 0.5 200 Exponential 0.1) 0
      p = mouseX kr 0 (bufFrames kr playBufNum) Linear 0.1

pbEx06 :: UGen
pbEx06 =
    out 0 $ playBuf 1 playBufNum rate 1 0 Loop DoNothing
    where
      rate = xLine kr 0.1 100 60 RemoveSynth

pbEx07 :: UGen
pbEx07 =
    out 0 $ playBuf 1 playBufNum (bufRateScale kr playBufNum * rate) 1 0
    Loop DoNothing
    where
      rate = fSinOsc kr (xLine kr 0.2 8 30 DoNothing) 0 * 3 + 0.6

pbEx08 :: IO UGen
pbEx08 = do
  rate <- (* 2) <$> lfNoise2 kr (xLine kr 1 20 60 DoNothing)
  return $ out 0 $ playBuf 1 playBufNum (bufRateScale kr playBufNum * rate)
         1 0 Loop DoNothing

--
-- Granular Synthesis
--

granularBufNum :: Num a => a
granularBufNum = 101

allocReadGranular :: FilePath -> IO OSC
allocReadGranular sf =
    withSC3 $ \fd -> do
      async fd $ b_free granularBufNum
      async fd $ b_allocRead granularBufNum sf 0 0

-- | tGrains: numChannels trigger bufnum rate centerPos dur pan amp interp
tgEx01 :: UGen
tgEx01 =
    out 0 $ tGrains 2 (impulse ar trate 0) granularBufNum 1 cpos dur 0 0.1 2
    where
      trate = mouseY kr 2 200 Linear 1
      cpos = mouseX kr 0 (bufDur kr granularBufNum) Linear 0.1
      dur = 4 / trate

-- | Why cant hear any sound with this?
-- ... Find it. Dont't know why but whiteNoise won't work with KR.
tgEx02 :: IO UGen
tgEx02 = do
  let trate = mouseY kr 8 120 Exponential 1
      dur = 1.2 / trate
      clk = impulse kr trate 0
      b = granularBufNum
      amp = 0.3
  pos <- pure (+ mouseX kr 0 (bufDur kr b) Linear 0.1) <*> tRand 0 0.01 clk
  pan <- (* 0.6) <$> whiteNoise ar
  return $ out 0 $ tGrains 2 clk b 1 pos dur pan amp 2

tgEx03 :: IO UGen
tgEx03 = do
  let b = granularBufNum
      trate = mouseY kr 2 120 Exponential 0.1
      dur = 1.2/trate
      clk = impulse kr trate 0
      pos = mouseX kr 0 (bufDur kr b) Linear 0.1
  n0 <- (* 3) <$> whiteNoise ar
  n1 <- (* 0.6) <$> whiteNoise ar
  let rate = shiftLeft 1.2 (roundE n0 1)
  return $ out 0 $ tGrains 2 clk b rate pos dur n1 0.25 2

tgEx04 :: IO UGen
tgEx04 = do
  let trate = mouseY kr 8 120 Exponential 0.1
      b = granularBufNum
      dur = 4 / trate
  clk <- dust kr trate
  pos <- pure (+ mouseX kr 0 (bufDur kr b) Linear 0.1) <*> tRand 0 0.01 clk
  pan <- pure (* 0.6) <*> whiteNoise ar
  return $ out 0 $ tGrains 2 clk b 1 pos dur pan 0.1 0

tgEx05 :: IO UGen
tgEx05 = do
  let trate = linExp (lfTri kr (mouseY kr 0.1 2 Linear 0.1) 0) (-1) 1 1 8 * 120
      dur = 12 / trate
      clk = impulse ar trate 0
      pos = mouseX kr 0 (bufDur kr b) Linear 0.1
      b = granularBufNum
  pan <- (* 0.6) <$> whiteNoise ar
  return $ out 0 $ tGrains 2 clk b 1 pos dur pan 0.1 1

tgEx06 :: IO UGen
tgEx06 = do
  let trate = 12
      dur = mouseY kr 0.2 24 Linear 0.1 / trate
      b = granularBufNum
      clk = impulse kr trate 0
  pos <- (+ mouseX kr 0 (bufDur kr b) Linear 0.1) <$> tRand 0 0.01 clk
  pan <- (* 0.6) <$> whiteNoise ar
  return $ out 0 $ tGrains 2 clk b 1 pos dur pan 0.1 1

tgEx07 :: IO UGen
tgEx07 = do
  let trate = 100
      dur = 8 / trate
      clk = impulse kr trate 0
      b = granularBufNum
  pos <- (\n -> integrator (n*0.001) 0.1) <$> brownNoise ar
  pan <- (* 0.6) <$> whiteNoise ar
  return $ out 0 $ tGrains 2 clk b 1 pos dur pan 0.1 1

tgEx08 :: IO UGen
tgEx08 = do
  let trate = mouseY kr 1 400 Exponential 0.1
      dur = 8 / trate
      clk = impulse kr trate 0
      pos = mouseX kr 0 (bufDur kr b) Linear 0.1
      b = granularBufNum
  pan <- (* 0.8) <$> whiteNoise ar
  rate <- (* 2) <$> whiteNoise ar
  return $ out 0 $ tGrains 2 clk b rate pos dur pan 0.1 1

tgEx09 :: IO UGen
tgEx09 = do
  let trate = mouseY kr 2 120 Exponential 0.1
      dur = 1.2 / trate
      clk = impulse ar trate 0
      b = granularBufNum
  rate <- (roundE 1 . (* 3) . (1.2 **)) <$> whiteNoise ar
  let pos = mouseX kr 0 (bufDur kr b) Linear 0.1
  pan <- (* 0.6) <$> whiteNoise ar
  let amp = 0.1
  return $ tGrains 2 clk b rate pos dur pan amp 2

--
-- GrainSin
--

gSinEx01 :: IO UGen
gSinEx01 = do
  let trigrate = mouseX kr 2 120 Linear 0.1
      winsize = recip trigrate
      trig = impulse ar trigrate 0
  freq <- tRand 440 880 trig
  pan <- lfNoise1 kr 0.2
  return $ out 0 $ grainSin 2 trig winsize freq pan (-1) * 0.2

grainSinBufNum :: Num a => a
grainSinBufNum = 102

allocGrainSinBuf :: IO ()
allocGrainSinBuf = withSC3 $ \fd ->
                   send fd $ b_alloc grainSinBufNum 1024 1

writeGrainSinBuf :: [Double] -> IO ()
writeGrainSinBuf vs =
    withSC3 $ \fd -> send fd $ b_setn grainSinBufNum [(0,vs)]

gSinEx02 :: IO UGen
gSinEx02 = do
  let trigrate = mouseX kr 2 120 Linear 0.1
      winsize = recip trigrate
      trig = impulse ar trigrate 0
  freq <- tRand 440 880 trig
  pan <- lfNoise1 kr 0.2
  return $ out 0 $ grainSin 2 trig winsize freq pan grainSinBufNum * 0.2

--
-- See also: GrainFM, GrainBuf, and GrainIn.
--

--
-- Controls
--
decayEx01 :: Control2 -> IO UGen
decayEx01 ug =
    (out 0 . (*0.2) . (* ug (impulse ar 1 0) 0.9)) <$> whiteNoise ar

decayEx02 :: Control2 -> IO UGen
decayEx02 ug =
    (\t n -> out 0 $ ug t 0.9 * 0.2 * n) <$> dust ar 3 <*> whiteNoise ar

decayEx03 :: Control2 -> IO UGen
decayEx03 ug =
    (\t -> out 0 $ sinOsc ar (ug t 0.5 * 800) 0 * 0.2) <$> dust ar 4

decay2Ex01 :: Control3 -> IO UGen
decay2Ex01 ug =
    (out 0 . (*0.2) . (* ug (impulse ar 1 0) 0.2 0.9)) <$> whiteNoise ar

decay2Ex02 :: Control3 -> IO UGen
decay2Ex02 ug =
    (\t n -> out 0 $ ug t 0.2 0.9 * 0.2 * n) <$> dust ar 3 <*> whiteNoise ar

lagEx01 :: Control2 -> UGen
lagEx01 ug = out 0 $ sinOsc ar (ug (lfPulse ar 2 0 0.5 * 800 + 400)
                                       (mouseX kr 0 0.5 Linear 0.1)) 0 * 0.2

integratorEx01 :: Control2 -> IO UGen
integratorEx01 ug =
    (\t -> out 0 $ sinOsc ar (ug t 0.99999 * 200 + 800) 0 * 0.2) <$> dust2 ar 8

--
-- Triggers
--

trigEx01 :: Trig2 -> IO UGen
trigEx01 ug =
    (\t -> out 0 $ ug t 0.2 * fSinOsc ar 800 0 * 0.4) <$> dust ar 2

tDelayEx01 :: Trig2 -> IO UGen
tDelayEx01 ug = do
  t <- dust ar 2
  return $ out 0 $
         mce [trig1 t 0.05 * fSinOsc ar 660 0 * 0.2,
              trig1 (ug t 0.1) 0.05 * fSinOsc ar 880 0 * 0.2]

latchEx01 :: Trig2 -> IO UGen
latchEx01 ug =
    (\n -> out 0 $ blip ar (ug n (impulse ar 9 0) * 400 + 500) 4 * 0.2)
    <$> whiteNoise ar

latchEx02 :: Trig2 -> UGen
latchEx02 ug =
    out 0 $ blip ar (ug (sinOsc ar 0.3 0) (impulse ar 9 0) * 400 + 500) 4 * 0.2

gateEx01 :: Trig2 -> IO UGen
gateEx01 ug =
    (\n -> out 0 $ blip ar (ug n (lfPulse ar 1 0 0) * 400 + 500) 4 * 0.2)
    <$> lfNoise2 ar 40

pulseCountEx01 :: Trig2 -> UGen
pulseCountEx01 ug =
    out 0 $ sinOsc ar (ug (impulse ar 10 0) (impulse ar 0.4 0) * 200) 0 * 0.05

pulseCountEx02 :: Trig2 -> IO UGen
pulseCountEx02 ug =
    (\r -> out 0 $ sinOsc ar (ug (impulse ar 12 0) r * 200) 0 * 0.05)
    <$> dust2 kr 4

pulseDividerEx01 :: Trig3 -> UGen
pulseDividerEx01 ug = out 0 $ mce [a, b]
    where
      p = impulse ar 8 0
      a = sinOsc ar 1200 0 * decay2 p 0.005 0.1
      b = sinOsc ar 600 0 * decay2 (ug p pdiv 0) 0.005 0.5
      pdiv = ceil (mouseX kr 1 8 Linear 0.1)


--
-- EnvGen
--

-- | EnvShape could be made from env, envCoord, envTrapezoid, envPerc,
-- envSine, etc.
--
-- * env [levels] [times] [curves] level-scale? level-offset?
-- * envCoord [(time,level)] total-duration level-scale envCurve
-- * envTrapezoid shape skew dur amp
-- * envPerc attack-time total-duration
-- * envSine total-duration max-level
--
egEx01 :: [UGen] -> UGen
egEx01 shape = out 0 $ sinOsc ar 880 0 * 0.2 * e
    where e = envGen kr 1 1 0 1 RemoveSynth shape

egEx02 :: [UGen] -> UGen
egEx02 shape = out 0 $ sinOsc ar 880 0 * 0.2 * e
    where e = envGen kr (impulse kr 2 0) 1 0 1 DoNothing shape

egEx03 :: [UGen] -> IO UGen
egEx03 shape = do
  t <- dust kr 3
  let e = envGen kr t 1 0 1 DoNothing shape
  return $ out 0 $ sinOsc ar 880 0 * 0.2 * e


-- | Hmmm... releasing of envelope wont work as expected. Some trick
-- working in sclang, or scsynth? Envelope won't release with merly sending
-- negative value to gate argument. ... Got it. The envelope used in
-- the target node should have next value to change. with havins value
-- 0 as its level, it sound like release. Probably forced releasing
-- means, "move to next value".
egEx04Def :: IO OSC
egEx04Def = withSC3 (sendSynthdef "egEx04" ug)
    where ug = out 0 $ sinOsc ar 880 0 * 0.2 * e
          e = envGen kr Arg.gate 1 0 1 RemoveSynth shape
          shape = envCoord [(0,0),(0.01,1),(0.2,0.5),(9999,0.0)] 1 1 EnvSin

-- | Send a synth with envelope gate opened.
egEx04 :: IO ()
egEx04 = withSC3 $ \fd -> do
           send fd $ s_new "egEx04" (-1) AddToTail 1 [("gate",1)]

-- | Release with specifyed duration. 0 means immediate release, 1
-- means move to 0 in 1 second.
egEx04Release :: Double -> IO ()
egEx04Release rt = withSC3 $ \fd -> send fd $ n_set (-1) [("gate",-1 -rt)]

egEx05 :: Int -> IO UGen
egEx05 n = do
 gen <- newStdGen
 let shape = envCoord (zip (times++[99999]) (levels++[0])) 1 1 EnvCub
     times = scanl (+) 0 $ take n $ randomRs (0.005,0.2) gen
     levels = 0 : (take (n-2) $ randomRs (0,1.0) gen)
     e = envGen kr ("gate" @= 1) 1 0 1 DoNothing shape
 return $ out 0 $ sinOsc ar 880 0 * 0.2 * e


--
-- Spectral
--

fftBuf :: Num a => a
fftBuf = 110

sndBuf :: Num a => a
sndBuf = 111

allocFFTBuf :: IO ()
allocFFTBuf = withSC3 $ \fd -> send fd $ b_alloc fftBuf 2048 1

allocReadSndBuf :: FilePath -> IO ()
allocReadSndBuf sf = withSC3 $ \fd -> send fd $ b_allocRead sndBuf sf 0 0

-- | Don't forget to allocate the buffer used for fft ugens.
fftEx01 :: UGen
fftEx01 = out 0 $ 0.5 * ifft' chain
    where
      chain = fft' fftBuf input
      input = playBuf 1 sndBuf (bufRateScale kr sndBuf) 1 0 Loop DoNothing

fftInput :: UGen
fftInput = playBuf 1 sndBuf (bufRateScale kr sndBuf) 1 0 Loop DoNothing

-- | Takes pv ugen and process it to soundfile played by playbuf.
-- To hear sound with doing nothing with pv ugens, try
-- @audition $ fftEx00 undefined const@.
fftEx00 :: UGen -> PV2 -> UGen
fftEx00 ctrl ug = out 0 $ 0.5 * ifft' chain
    where
      chain = ug (fft' fftBuf fftInput) ctrl

-- | For pv_MagAbove and  pv_MagBelow.
fftEx02 :: PV2 -> UGen
fftEx02 = fftEx00 (mouseX kr 0.1 512 Exponential 0.1)

-- | For pv_BrickWall
fftEx03 :: PV2 -> UGen
fftEx03 = fftEx00 (mouseX kr (-1) 1 Linear 0.1)

-- | For pv_MagFreeze
fftEx04 :: PV2 -> UGen
fftEx04 = fftEx00 (lfPulse kr 1 0.75 0.5)

-- | For pv_RandComb
fftEx05 :: PV3M -> IO UGen
fftEx05 ug = do
  chain <- ug (fft' fftBuf fftInput)
           (mouseX kr 0 1 Linear 0.1) (impulse kr 0.4 0)
  return $ out 0 $ 0.5 * ifft' chain

-- | For pv_RectComb, but not working due to audio buffer size?
fftEx06 :: PV4 -> UGen
fftEx06 ug = out 0 $ 0.5 * ifft' chain
    where chain = ug (fft' fftBuf fftInput) 8 (mouseY kr 0 1 Linear 0.1)
                  (mouseX kr 0 1 Linear 0.1)


--
-- Techniques
--

-- | Artificial Space ex01, correlated
asEx01a :: IO UGen
asEx01a = (out 0 . (* 0.2) . mce . replicate 2) <$> brownNoise ar

-- | Artificial Space ex01, not correlated
asEx01b :: IO UGen
asEx01b = (out 0 . (*0.2)) <$> clone 2 (brownNoise ar)

asEx02a :: IO UGen
asEx02a = (out 0 . (* 0.2) . mce . replicate 2 .
           (\n -> lpf n (mouseX kr 100 10000 Linear 0.1))) <$> brownNoise ar

asEx02b :: IO UGen
asEx02b = (out 0 . (* 0.2) . (\n -> lpf n (mouseX kr 100 10000 Linear 0.1)))
          <$> clone 2 (brownNoise ar)

-- asEx03a :: IO UGen
-- asEx03a = do
--   n <- (* 7e-3) <$> pinkNoise ar
--   let spec = klankSpec [200,671,1153,1723] [1,1,1,1] [1,1,1,1]
--       k = klank n 1 0 1 spec
--   return $ out 0 $ mce [k,k]

-- | Correlated
asEx03a :: IO UGen
asEx03a = (out 0 . (* 7e-3) . mce . replicate 2 . (\n -> klank n 1 0 1 spec))
           <$> brownNoise ar
    where spec = klankSpec [200,671,1153,1723] [1,1,1,1] [1,1,1,1]

-- | Not correlated
asEx03b :: IO UGen
asEx03b = (out 0 . (* 7e-3) . (\n -> klank n 1 0 1 spec)) <$>
          clone 2 (brownNoise ar)
    where spec = klankSpec [200,671,1153,1723] [1,1,1,1] [1,1,1,1]

-- | Two waves mixed together coming out both speakers
asEx04a :: UGen
asEx04a = out 0 . mce . replicate 2 . mix $
          varSaw ar (mce [100,101]) 0 0.1 * 0.2

-- | Two waves coming out each speaker independently
asEx04b :: UGen
asEx04b = out 0 $ varSaw ar (mce [100,101]) 0 0.1 * 0.2

-- | Delays as cues to direction
asEx05a :: UGen
asEx05a = out 0 $ mce . replicate 2 . (* lfTri ar 1000 0) $
          decay2 (impulse ar 4 0 * 0.2) 0.004 0.2

-- | Inter-speaker delays
asEx05b :: UGen
asEx05b = out 0 $ mce
          [delayC x 0.01 0.01,
           delayC x 0.02 (mouseX kr 0.02 0 Linear 0.1)]
    where
      x = lfTri ar 1000 0 * decay2 (impulse ar 4 0 * 0.2) 0.004 0.2

-- | Phasing sound
asEx06a :: IO UGen
asEx06a = do
  x <- (* 0.2) <$> brownNoise ar
  let x' = mix $ mce [delayC x 0.01 0.01,
                      delayC x 0.02 (mouseX kr 0 0.02 Linear 0.1)]
  return $ out 0 $ mce [x',x']

-- | Spatial phasing sound.
asEx06b :: IO UGen
asEx06b = do
  x <- (* 0.2) <$> brownNoise ar
  return $ out 0 $ mce
             [delayC x 0.01 0.01,
              delayC x 0.02 (mouseX kr 0.02 0 Linear 0.1)]


--
-- Parallel Structures
--  

psEx01 :: IO UGen
psEx01 = do
  let n = 16
      g a b = do
           f <- randomRIO (200,1200)
           return $ a + (fSinOsc ar f 0 / b)
  pure (out 0 . (* 0.2)) <*> foldM g 0 [1..n]

psEx02 :: IO UGen
psEx02 = do
  let n = 16
      g a b = do
           f <- randomRIO (200,1200)
           return $ a + (fSinOsc ar (f + mce [0,0.5]) 0 / b)
  pure (out 0 . (* 0.2)) <*> foldM g 0 [1..n]

psEx03 :: IO UGen
psEx03 = do
  let n = 16
      g a b = do
           af <- exp <$> randomRIO (log 0.1, log 1)
           ap <- randomRIO (0, pi * 2)
           pan <- randomRIO (-1,1)
           f <- exp <$> randomRIO (log 100, log 1000)
           let amp = max 0 (fSinOsc kr af ap)
           return $ a + pan2 (fSinOsc ar f 0 * amp / (2*b)) pan 1 
  pure (out 0 . (* 0.8)) <*> foldM g 0 [1..n]

psEx04 :: IO UGen
psEx04 = do
  let n = 8
      g a b = do
           gen <- newStdGen
           let (drate,_) = randomR (0.0001,1) gen
               (dtime,_) = randomR (0.0003, 0.0043) gen
               (pan,_) = randomR (-1,1) gen
           dt <- (* 0.3) <$> dust ar drate
           return $ a + pan2 (combL dt 0.01 dtime 4) pan 1
  out 0 <$> foldM g 0 [1..n]

mkps :: (Num a, Enum a, Monad m, Functor m) 
     => (UGen -> UGen) -> a -> (UGen -> a -> m UGen) -> m UGen
mkps f n g = (out 0 . f) <$> foldM g 0 [1..n]

psEx01' :: IO UGen
psEx01' = mkps (* 0.2) 16 $ \a b -> do
           f <- randomRIO (200,1200)
           return $ a + (fSinOsc ar f 0 / b)

psEx02' :: IO UGen
psEx02' = mkps (* 0.2) 16 $ \a b -> do
           f <- randomRIO (200,1200)
           return $ a + (fSinOsc ar (f + mce [0,0.5]) 0 / b)

psEx03' :: IO UGen
psEx03' = mkps (* 0.8) 16 $ \a b -> do
            af <- exp <$> randomRIO (log 0.1, log 1)
            ap <- randomRIO (0, pi * 2)
            pan <- randomRIO (-1,1)
            f <- exp <$> randomRIO (log 100, log 1000)
            let amp = max 0 (fSinOsc kr af ap)
            return $ a + pan2 (fSinOsc ar f 0 * amp / (2*b)) pan 1

psEx04' :: IO UGen
psEx04' = mkps id 16 $ \a b -> do
            gen <- newStdGen
            let (drate,gen') = randomR (0.0001,1) gen
                (dtime,gen'') = randomR (0.0003, 0.0043) gen'
                (pan,_) = randomR (-1,1) gen''
            dt <- (* 0.3) <$> dust ar drate
            return $ a + pan2 (combL dt 0.01 dtime 4) pan 1

