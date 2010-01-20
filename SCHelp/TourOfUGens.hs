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

