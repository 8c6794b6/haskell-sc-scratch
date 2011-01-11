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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_yetmorebloodyphones.html>
--
-- Try:
--
-- > > withSC3 reset
-- > > audition step1  -- move mouse x
-- > > audition step2  -- move mouse x and y
-- > > audition step4
-- > > audition step5
-- > > audition step6
-- > > withSC3 ringFmBell
--
module Pssd.TelephoneBells.YetMoreBloody where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import Pssd.Util

-- | Step 1. Control odds/evens with mouse x.
step1 :: UGen
step1 = out 0 $ mce [sig, sig] * 0.2
  where
    sig = cos (pi * phase)
    phase = sinOsc ar 300 0 + oes
    oes = mouseX kr 0 0.25 Linear 0.1

-- | Step 2. Control index with mouse y, odd/even mix
-- with mouse x.
step2 :: UGen
step2 = out 0 $ mce [sig, sig] * 0.2
  where
    sig = cos $ pi * phase
    phase = (sinOsc ar 300 0 * idx) + oes
    idx = mouseY kr 0 1 Linear 0.1
    oes = mouseX kr 0 1 Linear 0.1

-- | Step 4. FM sidebands.
step4 :: UGen
step4 = out 0 $ mce [sig, sig] * 0.2
  where
    sig = cos $ pi * phase
    phase = osc1 * osc2
    osc1 = linLin (lfSaw ar (-5) 0) (-1) 1 0 1 * sinOsc ar 2952 0
    osc2 = sinOsc ar 866 0 * 0.5

-- | Step 5. Index modulation.
step5 :: UGen
step5 = out 0 $ mce [sig, sig] * 0.2
  where
    sig = cos $ pi * phase
    phase = osc1 * osc2
    osc1 = (aSaw * sinOsc ar 2952 0) + aSaw
    aSaw = linLin (lfSaw ar (-5) 0) (-1) 1 0 1
    osc2 = sinOsc ar 866 0 * 0.5

-- | Step 6.
step6 :: UGen
step6 = out 0 $ mce [sig, sig] * 0.2
  where
    sig = mkFmBell aSaw i1 3031 1062 + mkFmBell aSaw i2 2952 866
    i1 = i2 * i2
    i2 = aSaw * aSaw
    aSaw = aSaw' * aSaw'
    aSaw' = linLin (lfSaw ar (-5) 0) (-1) 1 0 1

-- | Make an fm bell.
mkFmBell :: UGen -- ^ phase index offset for osc1
         -> UGen -- ^ phase index mul for osc1
         -> UGen -- ^ frequency for osc1
         -> UGen -- ^ frequency for osc2
         -> UGen
mkFmBell phr idx freq1 freq2 = cos (pi * phase) * 0.5
  where phase = ((idx * sinOsc ar freq1 0) + phr) *
                (sinOsc ar freq2 0 * 0.5)

-- | Fm bell-ish sound.
fmBell :: UGen
fmBell = out 0 $ mce [sig, sig] * 0.2
  where
    sig = mkFmBell aEnv i1 f1 f2 + mkFmBell aEnv i2 f3 f4
    i1 = i2 * i2
    i2 = aEnv * aEnv
    aEnv = linen t 0 1 1e-1 DoNothing
    f1 = ctrl "freq1" 3031
    f2 = ctrl "freq2" 1062
    f3 = ctrl "freq3" 2952
    f4 = ctrl "freq4" 866
    t = ctrl "t_trig" 0

-- | Trigger for fmBell.
hitFmBell :: UGen
hitFmBell = out outBus (sig * lfGate)
  where
    outBus = ctrl "outBus" 101
    sig = impulse kr (ctrl "freq" 5) (ctrl "phase" 0)
    lfGate = lfPulse kr (ctrl "gFreq" (1/4)) 0 0.5

-- | Hitting two bells.
ringFmBell :: (Transport t) => t -> IO ()
ringFmBell fd = do
  let trigBus1 = 101
      trigBus2 = 102
  async fd $ d_recv $ synthdef "fmBell" fmBell
  async fd $ d_recv $ synthdef "hitFmBell" hitFmBell
  _ <- s_new_id fd "hitFmBell" AddToTail 1
             [("freq", 23), ("phase", 0), ("outBus", fromIntegral trigBus1)]
  trig2Id <- s_new_id fd "hitFmBell" AddToTail 1
             [("freq", 17), ("phase", 1), ("outBus", fromIntegral trigBus2)]
  bell1Id <- s_new_id fd "fmBell" AddAfter trig2Id []
  bell2Id <- s_new_id fd "fmBell" AddAfter trig2Id
             [("freq1",3031) ,("freq2",1067)
             ,("freq3",2922) ,("freq4",850) ,("iPhase",1)]
  send fd $ n_map bell1Id [("t_trig", trigBus1)]
  send fd $ n_map bell2Id [("t_trig", trigBus2)]
