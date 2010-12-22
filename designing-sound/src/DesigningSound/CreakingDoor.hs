------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Creaking door
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Creaking_door>
--
-- /Example/:
--
-- > > audition $ out 0 $ pan2 (woodFilter (lpf (dust 'a' ar 10) 10000)) 0 1
-- > > let x = mouseX kr 0 1 Linear 0.1
-- > > audition $ out 0 $ creakingDoor x
--
module DesigningSound.CreakingDoor where

import Sound.SC3
import Sound.SC3.ID

-- | Formants for a wooden door
--
-- > ~woodfilter = { |input|
-- >   var freqs, rqs, output;
-- >   // Note: these freqs are as given in the diagram:
-- >   freqs = [62.5, 125, 250, 395, 560, 790];
-- >   // The Q values given in the diagram (we take reciprocal, since that's what BPF unit wants)
-- >   rqs   = 1 / [1, 1, 2, 2, 3, 3];
-- >   // in the text, andrew says that the freqs follow these ratios,
-- >   // which give a very different set of freqs...:
-- >   // freqs = 125 * [0.5, 1, 1.58, 2.24, 2.92, 2, 2.55, 3.16];
-- >
-- >   //Now let's apply the parallel bandpass filters, plus mix in a bit of the original:
-- >   output = BPF.ar(input, freqs, rqs).sum + (input*0.2);
-- > };
--
woodFilter :: UGen -> UGen
woodFilter input = output
  where
    output = mix (bpf input freqs rqs) + (input * 0.2)
    freqs = mce [62.5, 125, 250, 395, 560, 790]
    rqs = mce $ map (1/) [1, 1, 2, 2, 3, 3]

-- | Play woodFilter with low pass filtered dust sound.
--
-- > x = { Pan2.ar(~woodfilter.value(LPF.ar(Dust.ar(10), 10000))) }.play;
--
playWoodFilter :: UGen
playWoodFilter= out 0 $ pan2 (woodFilter (lpf (dust 'a' ar 10) 10000)) 0 1

-- | Stick-slip motion in response to applied force
--
-- > ~stickslip = { |force|
-- >   var inMotion, slipEvents, forceBuildup, evtAmp, evtDecayTime, evts;
-- >   force = force.lag(0.1); // smoothing to get rid of volatile control changes
-- >
-- >   inMotion = force > 0.1; // static friction: nothing at all below a certain force
-- >
-- >   // slip events are generated at random with freqency proportional to force.
-- >   // I originally used Dust to generate random events at a defined frequency, but
-- >   // that lacks the slight "pitched" sound of the creaky door. Here we use Impulse
-- >   // to generate a frequency, but we add some noise to its frequency to try and
-- >   // avoid it getting too perfectly regular.
-- >   slipEvents = inMotion * Impulse.ar(force.linlin(0.1, 1, 1, 1/0.003) *
-- >                LFDNoise1.ar(50).squared.linexp(-1,1, 0.5, 2).poll);
-- >
-- >   forceBuildup = Phasor.ar(slipEvents, 10 * SampleDur.ir, 0, inf).min(1);
-- >
-- >   // Whenever a slip event happens we use Latch to capture the amount of
-- >   // force that had built up.
-- >   evtAmp = Latch.ar(Delay1.ar(forceBuildup.sqrt), slipEvents);
-- >   evtDecayTime = evtAmp.sqrt;
-- >   // The book applies square-root functions to shape the dynamic range of the events.
-- >   // Remember that square-root is computationally intensive, so for efficient
-- >   // generation we might want to change it to (e.g.) a pre-calculated envelope.
-- >
-- >   // Now we generate the events
-- >   evts = EnvGen.ar(Env.perc(0.001, 1), slipEvents, evtAmp, 0, evtDecayTime * 0.01);
-- > };
--
stickSlip :: UGen -> UGen
stickSlip force = evts
  where
    evts = envGen ar slipEvents evtAmp 0 (evtDecayTime * 0.01) DoNothing shape
    shape = envPerc 0.001 1
    evtDecayTime = sqrt evtAmp
    evtAmp = latch (delay1 (sqrt forceBuildup)) slipEvents
    slipEvents = inMotion *
                 impulse ar
                   (linLin force' 0.1 1 1 (1/0.003) *
                    linExp (squared (lfdNoise1 'a' ar 50)) (-1) 1 0.5 2)
                   1
    forceBuildup = min (phasor ar slipEvents (10*sampleDur) 0 9e9 0) 1
    inMotion = force' >* 0.1
    force' = lag force 0.1

-- | Parallel delays simulate rectangular door frame.
--
-- > ~squarepanel = { |input|
-- >   var times, filt;
-- >   // times in milliseconds, converted to seconds:
-- >   times = [4.52, 5.06, 6.27, 8, 5.48, 7.14, 10.12, 16] * 0.001;
-- >   filt = DelayC.ar(input, times, times).mean;
-- >   filt = HPF.ar(filt, 125);
-- >   filt * 4
-- > };
--
-- Haskell implementation differs that its not using @mean@.
--
squarePanel :: UGen -> UGen
squarePanel input = filt * 0.05
  where
    filt = hpf filt' 125
    filt' = sum (map mkFilt times) / 16
    mkFilt t = delayC input t t
    times = map (* 0.001) [4.52, 5.06, 6.27, 8, 5.48, 7.14, 10.12, 16]

-- | Putting it all together.
--
-- > x = {~squarepanel.value(~woodfilter.value(~stickslip.value(MouseX.kr(0,1))))}.play
--
creakingDoor :: UGen -> UGen
creakingDoor = squarePanel . woodFilter . stickSlip