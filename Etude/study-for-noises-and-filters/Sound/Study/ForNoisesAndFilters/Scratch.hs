{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with noises and filters, scratch 1.
--
module Sound.Study.ForNoisesAndFilters.Scratch where

import Control.Monad (forM_)
import qualified Data.List as L
import Data.Map ((!))
import Data.Traversable (sequenceA)
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import qualified Sound.SC3.Monadic as N
import Sound.SC3.Lepton

grp = Group
syn = Synth

w = withSC3

-- n_mapa n ps = Message "/n_mapa" $ reverse $ L.foldl' f [Int n] ps
--   where
--     f b (n,i) = Int i:String n:b

b001Graph =
  grp 1
    [grp 10 []
    ,grp 11 []]

n001 = out 0 sig
  where
    sig = pan2 nf 0 1
    nf = resonz (n*0.5) freq rq
    n = whiteNoise 'a' ar
    rq = abs $ lfdNoise3 'q' kr 0.1
    freq = 8000 * abs (lfdNoise3 'f' kr 0.1)

n002 :: (Char -> Rate -> t) -> (t -> UGen -> UGen -> UGen) -> UGen
n002 noisef filterf = pan2 n' 0 1
  where
    n' = filterf n freq rq
    n = noisef 'a' ar
    freq = 8000 * abs (lfdNoise3 'f' kr 0.1)
    rq = abs $ lfdNoise3 'q' kr 0.1

n0021 = out 0 $ n002 whiteNoise resonz *
        decay2 (dust 'd' kr (abs (lfdNoise3 'f' kr 0.1) * 50)) 5e-4 100e-3

n003 = out 0 $ pan2 sig 0 1 * 0.05
  where
    sig = sum (map mkS [110,220..4400]) * hit
    hit = decay2 tr 5e-3 100e-3
    tr = impulse kr (50 * abs (lfdNoise3 'f' kr 1)) 0
    mkS f = resonz n f rq
    n = whiteNoise 'a' ar
    rq = (sinOsc kr 0.25 0 + 1) * 0.25

m001 = out 0 sig
  where
    sig = moogFF (n*0.1) y x 0
    y = mouseY kr 100 10000 Exponential 0.1
    x = mouseX kr 0 4 Linear 0.1
    n = whiteNoise 'a' ar

p001 = out 0 sig
  where
    sig = pluck (n*0.25) t dl (dl*y) 10 x
    t = dust 't' kr 9
    x = mouseX kr (-0.999) 0.999 Linear 0.1
    y = mouseY kr 0.1 1 Linear 0.1
    dl = 1 / 440
    n = whiteNoise 'a' ar

p002 = do
  let n = 128
  f <- clone n $ N.rand 0.05 0.2
  p <- clone n $ N.rand 0 1
  w <- clone n $ N.whiteNoise ar
  fi <- clone n $ N.rand 10 12
  coef <- N.rand 0.01 0.2
  l <- clone n $ N.rand (-1) 1
  let sig = leakDC (mix $ pan2 ks l 1) 0.995
      ks = pluck (w*0.1) i 0.01 (1/o) 2 coef
      o = linLin (sinOsc kr f p) (-1) 1 x 3000
      x = mouseX kr 60 1000 Exponential 0.1
      i = impulse kr fi 0
  return $ out 0 sig

so001 = out 0 $ sos (lfSaw ar 200 0 * 0.1) 1 0 0 b1 b2
  where
    b1 = 2 * rho * cos theta
    b2 = - (rho * rho)
    rho = line kr 0.6 0.99 5 RemoveSynth
    theta = line kr (0.2 * pi) pi 5 RemoveSynth

rz001 = out 0 sig
  where
    sig = resonz source 1200 q * amp
    source = pulse ar 440 0.5 * 2
    amp = decay2 tr 5e-4 250e-3
    q = decay2 tr 1e-3 800e-3
    tr = impulse kr 1 0

rz002 = out 0 $ resonz n f q
  where
    n = whiteNoise 'a' ar
    f = lfNoise0 'f' kr 4 * 110 + 660
    q = mce [0.005, 0.005]

rz003 = out 0 $ mce [sig,sig]
  where
    sig = f2 (f1 n)
    f1 i = resonz i 8000 0.5
    f2 i = resonz i 440 0.5
    n = whiteNoise 'n' ar

rlp001 = out 0 sig
  where
    sig = rlpf n f q
    n = saw ar (mce [220,221] + (lfNoise0 'n' kr 1 * 100 + 200)) * 0.2
    f = mce [lfNoise0 'r' kr 4 * 600 + 2400
            ,lfNoise0 'l' kr 3 * 600 + 2400]
    q = 0.1

kl001 = out 0 sig
  where
    sig = klank (pinkNoise 'd' ar * 0.01) 1 0 1 s
    s = klankSpec [800, 1071, 1153, 1732] [1,1,1,1] [1,1,1,1]

ktl001 = out 0 $ mce2 sig sig
  where
    sig = pinkNoise 'p' ar * burstEnv
    burstEnv = envGen kr tr 1 0 1 DoNothing $ envPerc 0 1
    tr = impulse kr 1 0

ktl002 = out 0 $ mce [sig,sig]
  where
    sig = combL nz dlyT dlyT dcyT + nz
    nz = pinkNoise 'p' ar * burstEnv
    burstEnv = envGen kr 1 1 0 1 DoNothing $ envPerc 0 1
    dlyT = 0.5
    dcyT = 10

ktl003 = out 0 sig
  where
    sig = (combL nz dlyT dlyT dcyT + nz) * 0.1
    nz = mce [grayNoise 'p' ar, pinkNoise 'p' ar] * burstEnv
    burstEnv = envGen kr tr 1 0 1 DoNothing $ envPerc 0.01 0.05
    tr = impulse kr tfreq 0
    tfreq = mouseY kr 1 20 Exponential 0.1
    dlyT = recip (midiCPS 69)
    dcyT = mouseX kr 0.25 10 Exponential 0.1

ktl004 = ktl004' (ctrl "dt" (1/440))
ktl004' dt = mrg [out 0 $ pan2 plk 0 1]
  where
    det = detectSilence plk 0.1 0.2 RemoveSynth
    plk = combL nz dt dt dcyT + nz
    nz = pinkNoise 'p' ar * burstEnv
    burstEnv = envGen kr 1 1 0 1 DoNothing shape
    shape = env [0,0,1,0] [0,1e-3,50e-3] [EnvNum (-13)] (-1) 0
    dcyT = 0.5

-- | Karplus strong syntesis example from:
--
-- * <http://swiki.hfbk-hamburg.de:8888/MusicTechnology/667>
--
ktl005 :: UGen
ktl005 =
  ktl005' (ctrl "ldt" (1/440)) (ctrl "rdt" (1/441)) (ctrl "lamp" 0.5)
    (ctrl "ramp" 0.5) (ctrl "artic" 1)
ktl005' ldt rdt lamp ramp artic = mrg [out 0 sig, l]
  where
    sig = (rlpf sig' (lfNoise1 'a' kr (mce [0.5, 0.43]) * 2000 + 2100) 0.1) * 0.6
    sig' = combL nz 1.0 (mce [ldt, rdt]) artic + nz
    nz = whiteNoise 'p' ar * burstEnv
    burstEnv = envGen kr 1 1 0 1 DoNothing shape * mce [lamp, ramp]
    shape = env [0,0,1,0] [0,1e-3,2e-3] [EnvNum (-13)] (-1) 0
    l = line kr 0 1 artic RemoveSynth

rev001 :: UGen
rev001 = replaceOut input sig
  where
    input = in' 2 ar (ctrl "out" 0)
    sig = foldr f input "abcdefg"
    f a b = allpassN b 0.01 (rand a 0.005 0.01) 4

setupKtl :: (Transport t) => t -> IO ()
setupKtl fd = do
  mapM_ (\(n,u) -> loadSynthdef n u fd)
    [("pluck",ktl005),("rev001", rev001),("cnv002", cnv002)]
  flip (addNode 0) fd $
    grp 1
      [grp 10 []
      ,grp 11 [syn 1100 "rev001" []]
      ,grp 12 []]

nsetP nid ps = do
  ms <- act $ runPIO $ sequenceA $ M.fromList ps
  forM_ ms $ \m -> do
    act $ utcr >>= \t -> w $ \fd ->
      send fd $ Bundle (UTCr (t+0.1)) [n_set nid (M.assocs m)]
    rest (m!"del")
    pauseHere

snewP name aa gid ps = do
  ms <- act $ runPIO $ sequenceA $ M.fromList ps
  forM_ ms $ \m -> do
    act $ utcr >>= \t -> w $ \fd ->
      send fd $ Bundle (UTCr (t+0.1)) [s_new name (-1) aa gid (M.assocs m)]
    rest (m!"del")
    pauseHere
  Sound.SC3.Lepton.done

-- | Try:
--
-- > > e <- initEnv
-- > > tadd (tu 4) e "act1" act1
-- > > tadd (tu 4) e "act2" act2
--
-- When had enough:
--
-- > > mapM_ (tkill0 e) ["act1", "act2"]
--
act1 :: Act ()
act1 = do
  let ps = [0,3,5,7,10]
      dt = fmap (recip . midiCPS) $
           pforever $ pchoose 1 ps + pchoose 1 [36,48,60,72]
  snewP "pluck" AddToTail 10
    [("ldt", dt)
    ,("rdt", dt)
    ,("lamp", pforever $ pchoose 1 [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95])
    ,("ramp", pforever $ prange 0 1.0)
    ,("del", pforever $ pchoose 1 (map (pval . recip . (2^)) [0..8]) {- prepeat 0.125 -})]

act2 :: Act ()
act2 = do
  ps <- act $ runPIO $ pforever $ pshuffle
        [pval [48,60,67,75]
        ,pval [48,60,65,80]]
  let f xs = forM_ xs $ \x -> do
        act $ utcr >>= \t -> w $ \fd ->
          send fd $ Bundle (UTCr (t+0.1))
            [s_new "cnv002" (-1) AddToTail 10
             [("freq",midiCPS x)]]
             -- [("ldt",recip $ midiCPS x)
             -- ,("rdt",recip $ midiCPS x)
             -- ,("lamp",0.5)
             -- ,("ramp",0.5)
             -- ,("artic",2)]]
  mapM_ (\xs -> f xs >> rest 2) ps

cnv001 = out 0 $ mce [sig, sig]
  where
    sig = convolution tck krn 2048
    krn = mix $ pulse ar (mce [110,220..1320])
          (lfdNoise3 'p' kr 2 + 1 * 0.5) * 0.25
    tck = envGen kr trg 1 0 1 DoNothing shape * nz
    shape = env [0,0,1,0] [0,250e-3,600e-3]
            [EnvNum (-13),EnvNum (-8),EnvNum (-8)] (-1) 0
    nz = pinkNoise 'n' ar * 0.1
    trg = dust 't' kr 4

cnv002 = cnv002' ("freq"@@440)
cnv002' freq = mrg [l, out 0 $ pan2 sig 0 1]
  where
    sig = convolution tck krn 4096
    krn = mix $ pulse ar freq (lfdNoise3 'p' kr 4 + 1 * 0.5) * 0.125
    tck = envGen kr 1 1 0 1 DoNothing shp * nz
    shp = env [0,0,1,0] [0,100e-3,800e-3] [EnvNum (-12)] (-1) 0
    nz = pinkNoise 'n' ar * 0.1
    l = line kr 0 1 1 RemoveSynth

cnv003 = out 0 $ mce [sig,sig]
  where
    sig = rlpf (convolution n k 2048) (lfdNoise3 'r' kr 1 * 4000 + 4100) q * 0.1
    q = lfdNoise3 'q' kr 0.5 * 0.4 + 0.5
    n = whiteNoise 'n' ar * 0.1 * hit
    hit = envGen kr tr 1 0 1 DoNothing shape
    shape = env [0,0,1,0] [0,2e-3,800e-3] [EnvNum (-13)] (-1) 0
    tr = impulse kr (lfdNoise3 'f' kr 0.25 * 3 + 3) 0
    k = mix (lfSaw ar (mce [freq,freq*2+0.11,freq*3-0.08,freq*4+0.07]) 0 *
             mouseX kr 1 2 Linear 0.1)
    freq = linExp (lfdNoise3 'a' kr 1) (-1) 1 10 1760

sos001 :: UGen
sos001 = out 0 $ mce [sig, sig]
  where
    sig = clip2 (sos n a0 a1 a2 b1 b2) 1 * 0.1
    n = whiteNoise 'n' ar
    a0 = f $ sinOsc kr 0.5 0.125
    a1 = f $ sinOsc kr 0.51 0.25
    a2 = f $ sinOsc kr 0.49 0.375
    b1 = f $ sinOsc kr 0.508 0.5
    b2 = f $ sinOsc kr 0.498 0.625
    f n = n * 0.5 + 0.5

sos002 :: UGen
sos002 = out 0 $ mce [sig, sig]
  where
    sig = sinOsc ar (vib*200+600) 0 * 0.2
    vib = sos (lfSaw kr 3.16 0) 1 0 0 b1 b2
    b1 = 2.0 * rho * cos theta
    b2 = negate $ squared rho
    rho = mouseY kr 0.6 0.99 Linear 0.1
    theta = mouseX kr (0.2*pi) pi Linear 0.1

sos003 :: UGen
sos003 = out 0 $ mce [sig, sig]
  where
    sig = sos (whiteNoise 'a' ar) 1 0 0 b1 b2 * 0.1
    b1 = 2.0 * rho * cos theta
    b2 = negate $ squared rho
    rho = mouseY kr 0.6 0.99 Linear 0.1
    theta = mouseX kr (0.2*pi) pi Linear 0.1

sos004 :: UGen
sos004 = out 0 $ mce [sig, sig]
  where
    sig = clip2 (f1 (f2 (f3 n)) * 0.1) 1
    n = whiteNoise 'a' ar
    f1 i = sos i 1 0.61803 1 0.60515 0.95873
    f2 i = sos i 1 (-0.61803) 1 (-1.58430) 0.95873
    f3 i = sos i 1 1 0 0.97915 0

nzGraph01 :: SCNode
nzGraph01 =
  grp 1
    [grp 10
      [syn 1000 "noise" []]
    ,grp 11
      [syn 1100 "filt2" []]]

setupFilt01 :: Transport t => t -> IO ()
setupFilt01 fd = do
  mapM_ (\(n,u) -> loadSynthdef n u fd)
    [("noise", nz003),("filt1",filt1),("filt2",filt2)
    ,("rg002",rg002)]
  addNode 0 nzGraph01 fd

mapARate :: Transport t => t -> IO ()
mapARate fd = do
  send fd $ n_mapa 1100 [("a_left",0),("a_right",1)]

nz003 = out 0 $ mce [sig,sig]
  where
    sig = whiteNoise 'a' ar * ("amp"@@0.1) * hit
    hit = decay2 tr 1e-3 300e-3
    tr = impulse kr (lfdNoise3 'd' kr 0.2 * 16 + 16) 1

filt1 = filt1' ("left"@@0) ("right"@@1)
filt1' l r = mrg [replaceOut 0 (f l), replaceOut 1 (f r)]
  where
    f n = sos (in' 1 ar n) 0 0 1 (cos theta) phi
    theta = mouseY kr (0.25*pi) pi Linear 0.1
    phi = mouseX kr 1e-9 0.995 Linear 0.1

filt2 = filt2' ("a_left"@@0) ("a_right"@@1)
filt2' l r = mrg [replaceOut 0 (f l), replaceOut 1 (f r)]
  where
    f n = sos n 1 (-2) 3 0 0

fmlt001 = out 0 $ mce [sig,sig]
  where
    sig = (f1 b + f2 b + f3 b) * 0.1
    f1 i = formlet i 700 0.1 0.2
    f2 i = formlet i 1220 0.1 0.51
    f3 i = formlet i 2600 0.1 0.31
    b = v + n
    -- v = blip ar (lfdNoise3 'b' kr 1 * 200 + 210) 100
    v = 0
    n = whiteNoise 'a' ar * decay2 tr 5e-3 125e-3
    tr = dust 't' kr 2.5

tish nz f = nz 'n' ar * decay2 (dust 't' kr f) 5e-3 125e-3

rg001 :: UGen
rg001 = out 0 $ mce [sig,sig]
  where
    sig = sig' * 0.08
    rf = lfdNoise3 'd' kr 0.3 * 3200 + 3210
    rq = lfdNoise3 'q' kr 0.23 * 0.4 + 0.5
    sig' = sum $ zipWith fs
           [12.328,58.23,119,223,709.892,1238,2680,4321.5678
           ,5678,4321,6821.3290,8881.9101]
           [2.0,1.91..]
    nz = whiteNoise 'n' ar * hit * 0.1
    hit = decay2 tr 1e-3 29e-3
    tr = impulse kr (lfdNoise3 'f' kr 1 * 12.5 + 13) 0
    fs fq dt = ringz nz fq dt

rg002 = out 0 sig''
  where
    sig = (mce [sigl, sigr] + pan2 sigc (lfdNoise3 'p' kr (1/16)) 1) * 0.5
    sigl = delayC sigc 1 (lfdNoise3 'l' kr (1/16) * 15e-3 + 16e-3)
    sigr = delayC sigc 1 (lfdNoise3 'r' kr (1/16) * 15e-3 + 16e-3)
    sigc = limiter (rhpf (sig'' + sig') 50 0.4 * 0.3) 1 0.2
    sig' = foldr (\a b -> combC b 0.5 (ttdlt a) (ttdct a)) sig'' [1..11]
    tt i lo hi = tExpRand i lo hi dtr
    ttdlt i = lag3 (tExpRand (i::Int) (recip 200) 1 dtt) 28e-3
    -- ttdlt i = lag3 (ttdlt0*i*rand 'p' 0.999 1.001) 25e-3
    -- ttdlt0 = tExpRand 't' (recip 50) (recip 2) dtt
    ttdct i = lag3 (tExpRand i 200e-3 1000e-3 dtt) 28e-3
    dtr = dust 't' kr 0.1
    dtt = impulse kr dttf 0
    dttf = lfdNoise3 'f' kr (1/128) * 1.5 + 1.6
    sig'' = sum $ zipWith3 rs fs ts as
    -- sig'' = nz
    fs = [tt 'a' 50 200, tt 'b' 200 800, tt 'c' 800 3200, tt 'd' 3200 12800]
    ts = [lfdNoise3 'b' kr 1 * 0.5 + 0.6
         ,lfdNoise3 'd' kr 1 * 0.8 + 0.9
         ,lfdNoise3 'a' kr 1 * 0.5 + 0.6
         ,lfdNoise3 'c' kr 1 * 0.8 + 0.9]
    as = [lfdNoise3 'c' kr 1 * 0.25 + 0.24
         ,lfdNoise3 'a' kr 1 * 0.25 + 0.24
         ,lfdNoise3 'b' kr 1 * 0.25 + 0.24
         ,lfdNoise3 'd' kr 1 * 0.25 + 0.24]
    nz = lorenzL ar sampleRate n0 n1 n2 0.05 0.1 0 0 * 0.3 -- * hit
    -- nz = whiteNoise 'a' ar * 0.1
    -- nz = pinkNoise 'a' ar * 0.1
    -- nz = brownNoise 'w' ar * 0.1
    n0 = n * 8 + 10
    n1 = n * 30 + 38
    n2 = n * 1.5 + 2
    n = lfdNoise3 'n' kr 1
    hit = envGen kr tr 1 0 1 DoNothing shp
    shp = env [0,0,1,0] [0,8e-3,278e-3] [EnvNum (-13)] (-1) 0
    -- tr = impulse kr ((sinOsc kr (1/16) 0 * 2 + 2)+ lfdNoise3 'f' kr 1 * 1.5 + 2) 0
    tr = coinGate 'c' 0.5 (impulse kr (henonL kr (1/2) 1.4 0.3 0 0.05 * 2.5 + 3.5) 0)
    rs f t a = ringz nz f t * a

lz001 = out 0 $ mce [o,o]
  where
    o = lorenzL ar sampleRate n0 n1 n2 0.05 0.1 0 0 * 0.2
    n0 = n * 2 + 10
    n1 = n * 20 + 38
    n2 = n * 1.5 + 2
    n = lfdNoise3 'n' kr 1

-- Cottle chapter 10 - Karplus/Strong example from:
--
-- <http://swiki.hfbk-hamburg.de:8888/MusicTechnology/667>
--

-- // 10.1 noise burst

-- s = Server.internal.boot;

-- // Repeated triggers so you can see the scope better.

-- (
-- { //Beginning of Ugen function
--      var burstEnv, trig, trigFreq = 1, att = 0, dec = 1; //Variable declarations
--      trig = Impulse.kr(trigFreq);
--      burstEnv = EnvGen.kr(Env.perc(att, dec), trig); //Define envelope
--      PinkNoise.ar(burstEnv); //Noise, amp controlled by burstEnv
-- }.scope;	//End Ugen function
-- )

-- // 10.2 burst and delay

-- (
-- { //Beginning of Ugen function
--      var burstEnv, att = 0, dec = 1; //Variable declarations
--      var out, delayTime = 0.5, delayDecay = 10;
--      burstEnv = EnvGen.kr(Env.perc(att, dec)); //Define envelope
--      out = PinkNoise.ar(burstEnv); //Noise burst
--      CombL.ar(
--              out,
--              delayTime,
--              delayTime,
--              delayDecay,
--              add: out); //Echo chamber
-- }.scope;	//End Ugen function
-- )

-- // 10.3 reciprocal

-- // Note that postln is removed. The interpreter posts the result of the last expression automatically.

-- 440.reciprocal;

-- // 10.4 midi to cps to reciprocal

-- 69.midicps.reciprocal;

-- // 10.5 pluck

-- (
-- { //Beginning of Ugen function
--      var burstEnv, att = 0, dec = 0.05; //Variable declarations
--      var drySignal, delayTime, delayDecay = 10;
--      var midiPitch = 69; // A 440
--      delayTime = midiPitch.midicps.reciprocal;
--      burstEnv = EnvGen.kr(Env.perc(att, dec)); //Define envelope
--      drySignal = PinkNoise.ar(burstEnv); //Noise burst
--      CombL.ar(drySignal, delayTime, delayTime,
--              delayDecay, add: drySignal); //Echo chamber
-- }.scope; //End Ugen function
-- )

-- // 10.6 Spawn and pluck

-- // Now we have a style that's more amenable to sc3:
-- // write the instrument into a func, then spawn it.
-- // You've seen this before and you can probably
-- // convert it yourself by this point... but, to reiterate,
-- // make the instrument func into a SynthDef, then
-- // play the synthdef in a Routine or Task.

-- // But... he's doing his pitch calculations in the synthdef.
-- // It's faster to do all that on the client side and pass it in.

-- // Rather than apply doneAction to the overall envelope,
-- // in this case it's better to detect silence to kill the synth.

-- (
-- SynthDef("pluck", { //Beginning of Ugen function
--      arg delayTime;
--      var burstEnv, att = 0, dec = 0.05; //Variable declarations
--      var out, delayDecay = 0.5, midiPitch;
--      burstEnv = EnvGen.kr(Env.perc(att, dec)); //Define envelope
--      out = PinkNoise.ar(burstEnv); //Noise burst
--      out = CombL.ar(out, delayTime, delayTime,
--              delayDecay, add: out); //Echo chamber which produces pitch
--      DetectSilence.ar(out, doneAction:2);
--      Out.ar(0, out)
-- }).send(s);

-- r = Task({
--      {	0.25.wait;
--              Synth("pluck", [\delayTime, rrand(32, 55).midicps.reciprocal], target:s);
--      }.loop;
-- }).play(SystemClock)
-- )

-- r.stop;

-- // Just For Fun: Karplus-Strong Patch

-- // 10.7 expanded pluck

-- // Most of the early part of the his instrument function is
-- // actually control and should be shifted to the Routine.
-- // It's also kind of sloppy to spawn the reverb. A true
-- // reverb runs external to the notes being played, so I
-- // will revise it that way here.

-- // DetectSilence isn't so good here because if both
-- // channels are silent from the start, it won't kill the synth.
-- // So I'll use a line, which also has a doneAction.

-- (
-- SynthDef("pluck", {
--      arg	lDelayTime, rDelayTime, lAmp, rAmp, articulation;
--      var burstEnv, att = 0, dec = 0.05; //Variable declarations
--      var out;
--      //The mul value (amplitude) for the envelope is set to either 1 (on)
--      //or 0 (off). This is done for both channels.
--      burstEnv = EnvGen.kr(Env.perc(att, dec)) * [lAmp, rAmp]; //Define envelope
--      out = PinkNoise.ar(burstEnv); //Noise burst
--      out = CombL.ar(out, lDelayTime.max(rDelayTime), [lDelayTime, rDelayTime],
--              articulation, add: out); //Echo chamber
-- //Filter -- each note can have a unique filter, that's OK
--      out = RLPF.ar(out, LFNoise1.kr([0.5, 0.43], 2000, 2100), 0.5);
--      Line.kr(0, 1, articulation, doneAction:2);
--      Out.ar(0, out) //return this value
-- }).send(s); //End Ugen function

-- v = SynthDef("reverb2chan", {
--      arg bus, out;
--      out = In.ar(bus, 2);
--      2.do({out = AllpassN.ar(out, 0.01, rrand(0.005, 0.01), 4)});
--      ReplaceOut.ar(bus, out);
-- }).play(s, [\bus, 0]);	// go ahead and start it now

-- r = Task({
--      var midiPitch, delayTime, legalPitches, articulation, count = 0, lAmp, rAmp;
--      legalPitches = [0, 2, 4, 6, 8, 10]; //whole tone scale
--      articulation = [0.125, 0.25, 0.5, 1.0].choose;
--      {	0.125.wait;
--              //midiPitch is set to a L&R array of one of the legalPitch choices, plus
--              //an octave. The left channel wraps through the choices.
--              // only one of these is chosen in the Synth call below. This is what
--              // would have happened in sc2 as well, by multiplying one channel by 0
--              // and the other by 1.
--              midiPitch = [legalPitches.choose + [36, 48, 60].choose,
--                                      legalPitches.wrapAt(count) + [36, 48].choose];
--              count = count + 1; //Count is used with wrapAt above
--      //	[midiPitch, count].postln; //For checking values
--              delayTime = midiPitch.midicps.reciprocal; //Calculate reciprocal
--              articulation = [0.125, 0.25, 0.5, 1.0].choose;
--                      // this syntax means assign the first array element to lAmp and the 2nd to rAmp
--              #lAmp, rAmp = [2.rand, 2.rand];
--              Synth.head(s, "pluck", [\lDelayTime, delayTime[0], \rDelayTime, delayTime[1],
--                      \lAmp, lAmp, \rAmp, rAmp, \articulation, articulation]);
--      }.loop;
-- }).play(SystemClock);
-- )

-- r.stop;
-- v.free;
