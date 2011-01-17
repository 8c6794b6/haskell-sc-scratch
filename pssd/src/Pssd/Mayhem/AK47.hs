------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_AK47.html>
--
--
-- Try:
--
-- > > withSC3 reset
-- > > withSC3 loadAK47
--
module Pssd.Mayhem.AK47 where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Try:
--
-- > > audition $ sweepImpulse1 20000 0 0.5
-- > > audition $ sweepImpulse1 20000 0 0.0025
--
sweepImpulse1 :: UGen -- ^ Start frequency
              -> UGen -- ^ End frequency
              -> UGen -- ^ Sweep duration in second
              -> UGen
sweepImpulse1 start end dur = out2 (sig * 0.25)
  where
    sig = sinOsc ar freq 0
    freq = line ar start end dur RemoveSynth

-- | Cubed frequency sweep from 20000 to 0.
--
-- > > audition sweepImpulse2
--
sweepImpulse2 :: UGen
sweepImpulse2 = out2 (sig * 0.25)
  where
    sig = sinOsc ar freq 0
    freq = cubed (line ar 1 0 0.025 RemoveSynth) * 20000

-- | Multiplied with white noise.
--
-- Using @lfSaw@ as replacement of @phasor~@ in pd.
-- This UGen does not contain out.
--
-- > > audition $ out2 sweepImpulse3
--
sweepImpulse3 :: UGen
sweepImpulse3 = sig * amp
  where
    sig = cos ((oscil + noise) * pi)
    amp = squared baseLine
    noise = lpf (whiteNoise 'a' ar * amp) 1 * baseLine
    oscil = lfSaw ar freq 0
    freq = cubed baseLine * 5000 + 100
    baseLine = cubed $ linen (ctrl "t_trig" 1) 0 1 0.025 DoNothing

-- | Resonated impulse.
--
-- > > audition $ out2 resonatedImpulse
--
resonatedImpulse :: UGen
resonatedImpulse = sig
  where
    sig = bpf s0 2500 5 * 0.33
    s0 = resonate sweepImpulse3

-- | Apply multiple band pass filter and sum, to make formant.
--
-- Using low pass filter rather than band pass filter.
--
resonate :: UGen -> UGen
resonate ug = sig
  where
    sig = sum [ lpf ug (3600*mul) * 4
              , lpf ug (4600*mul) * 4
              , lpf ug (2620*mul) * 6
              , lpf ug (1220*mul) * 5
              , lpf ug (945*mul) * 10
              , lpf ug 800 * 12]
    mul = 0.9

-- | Shell detonation.
--
-- > > audition shellDetonation
--
shell :: UGen
shell = out2 sig
  where
    sig = clip n0 (-1) 1 * ctrl "amp" 1
    n0 = resonate (n1+sweepImpulse3)
    n1 = n2 * nLine
    n2 = hpf (clip (whiteNoise 'b' ar * 100) (-0.3) 0.3 * nLine) 12
    nLine = squared $ cubed $ cubed $
            linen (delayN (ctrl "t_trig" 1) 1 0.01) 0 1 1.2 DoNothing

-- | AK47 Gas venting, take 1.
--
-- Again, using low pass instead of bandpass in several flows.
gas :: UGen
gas = out2 $ sig * ctrl "amp" 0.3
  where
    sig = sig1 + sig2 + sig3
    sig1 = sig0 * 5
    sig2 = hpf sig15 30
    sig3 = lpf sig15 400
    sig15 = clip sig0 (-0.1) 0.2 * 5
    sig0 = sum [ lpf sig0' 400
               , lpf sig0' 800
               , lpf sig0' 1600 ]
    sig0' = sos oscil 1 (-0.677165) 1.84252 (-0.866142) (-0.0787402)
    oscil = (sinOsc ar freq 0 * baseEnv) + noise
    freq = sinOsc ar (squared baseEnv * 1000) 0 * 1000
    baseEnv = linen (ctrl "t_trig" 1) 0 1 0.01 DoNothing
    noise = lpf (whiteNoise 'a' ar ) 900 * cubed noiseEnv * 0.02
    noiseEnv = linen (ctrl "t_trig" 1) 0 1 0.8 DoNothing

-- | Reload bolt.
reload :: UGen
reload = out2 $ sig * ctrl "amp" 1
  where
    sig = clip (bodyReson (slide trg + ratchet trg) * 3) (-0.6) 0.6
    trg = ctrl "t_trig" 1

-- | Body resonance of reload sound.
bodyReson :: UGen -- ^ In sound
          -> UGen
bodyReson ug = d1 + d2
  where
    d1 = bpf (delayN ug 0.1 0.05) 1200 3 * 0.3
    d2 = bpf (delayN (ug + d1 * 0.3) 0.1 0.05) 1200 3 * 0.3

-- | Slide sound in reload.
slide :: UGen -- ^ Trigger
      -> UGen
slide t = bpf sig freq 0.4 * 0.3 * amp
  where
    sig = hpf (whiteNoise 'b' ar) 1000
    amp = squared baseLine
    freq = baseLine * 200 + 4500
    baseLine = envGen kr t 1 0 1 DoNothing $
               envCoord [(0,0), (0.2,1), (0,0)] 1 1 EnvLin

-- | Ratchet sound in reload.
ratchet :: UGen -- ^ Trigger
        -> UGen
ratchet t = select (tiRand 'i' 0 2 t) (mce [sig1, sig2, sig3])
  where
    sig1 = mkSig t1 [0.05, 0.01, 0.01] [4584, 4237, 4876] [0.3, 0.14, 0.12]
    sig2 = mkSig t2 [0.07, 0.045, 0.011] [345, 256, 11023] [0.22,0.3,0.46]
    sig3 = mkSig t3 [0.04, 0.022, 0.008] [5432, 6298, 2470] [0.4,0.2,0.2]
    mkSig trg ds fs qs = 0.33 * sum (zipWith3 (mkSig' trg) ds fs qs)
    mkSig' trg' d f q =
      hpf n 300 * cubed (cubed (envGen kr trg' 1 0 1 DoNothing
                         (envCoord [(0,0),(0,1),(d,0)] 1 1 EnvCub)))
    t1 = tDelay t 0.2
    t2 = tDelay t1 0.01
    t3 = tDelay t2 0.045
    n = whiteNoise 'd' ar

-- | Shell case tinkle
tinkle :: UGen
tinkle = out2 $ clip sig (-0.9) 0.9 * amp * ctrl "amp" 1
  where
    sig = 100 * (sum $ map (\f -> bpf n f (1/800)) [f1,f2,f3])
    f1 = tRand 'f' 4500 5500 trg
    f2 = f1 + tRand 'g' 700 800 trg
    f3 = f2 + tRand 'h' 300 400 trg
    amp = sqrt (tRand 's' 1 0.25 trg) * ampEnv
    ampEnv = cubed $
             envGen kr td1 1 0 1 DoNothing $
             envCoord [(0,1e-9),(0,1),(dur,1e-9)] 1 1 EnvCub
    dur = tRand 'u' 0.2 0.1 trg + 0.2
    td1 = delayN trg 1 (tRand 't' 0.25 0.75 trg)
    trg = ctrl "t_trig" 1
    n = whiteNoise 'a' ar

-- | UGen for controlling fiering of AK47.
--
-- Reloads once in 12 shots.
--
shooter :: UGen
shooter = mrg [out reloadBus rel, out shotBus shot']
  where
    shot = dust 'd' kr freq
    shot' = shot * setResetFF reloaded rel
    rel = tDelay (pulseDivider shot 12 0) 1
    reloaded = tDelay rel 1
    shotBus = ctrl "shotBus" 101
    reloadBus = ctrl "reloadBus" 102
    freq = ctrl "freq" 1

-- | Load synthdefs used in AK47.
loadAK47 :: (Transport t) => t -> IO ()
loadAK47 fd = do
  let dr (n, u) = async fd . d_recv $ synthdef n u
  mapM_ dr [("shell",shell)
           ,("gas",gas)
           ,("tinkle",tinkle)
           ,("shooter",shooter)
           ,("reload",reload)]
  mkTree ak47graph fd

-- | Synth graph for fireing AK47.
ak47graph :: SCNode
ak47graph =
  Group 0
    [Group 1
       [Group 11
         [Synth 1101 "shooter"
           ["freq":=2,"shotBus":=shotBus,"reloadBus":=reloadBus]]
       ,Group 21
          [Synth 2101 "shell"
            ["t_trig":<-shotBus,"amp":=0.3]
          ,Synth 2102 "gas"
            ["t_trig":<-shotBus,"amp":=1.5]
          ,Synth 2103 "tinkle"
            ["t_trig":<-shotBus,"amp":=0.1]
          ,Synth 2104 "reload"
            ["t_trig":<-reloadBus]]]]
  where
    shotBus, reloadBus :: Num a => a
    shotBus = 201
    reloadBus = 202
