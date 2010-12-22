------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Alarm tones.
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Alarms>
--
-- /Example/:
--
-- > > audition twoAlter
-- > > audition threeAlter
-- > > audition threeDemand
-- > > n <- audit "dsaf_multialarm" dsaf_multialarm
-- > > happyBlips n
-- > > invaders n
-- > > errorCode n
-- > > nfree n
--
module DesigningSound.Alarms where

import Sound.SC3
import Sound.SC3.ID

import DesigningSound.Util

-- | Alarm with two altering tones
--
-- > Ndef(\alarm, {
-- >   var tone1 = SinOsc.ar(600);
-- >   var tone2 = SinOsc.ar(800);
-- >   // We switch between the tones using LFPulse, but soften the crossfade with the low-pass:
-- >   var control = LPF.kr(LFPulse.kr(2), 70);
-- >   var out = SelectX.ar(control, [tone1, tone2]);
-- >   Pan2.ar(out * 0.1)
-- > }).play
--
twoAlter :: UGen
twoAlter = out 0 $ pan2 (o * 0.1) 0 1
  where
    o = select ctrl (sinOsc ar (mce [600, 800]) 0)
    ctrl = lpf (lfPulse kr 2 0 0.5) 70

-- | Alarm with three alternating tones
--
-- > Ndef(\alarm, {
-- >   var tone1 = SinOsc.ar(723);
-- >   var tone2 = SinOsc.ar(932);
-- >   var tone3 = SinOsc.ar(1012);
-- >   // Stepper is perfect for stepping through the options:
-- >   var control = LPF.kr(Stepper.kr(Impulse.kr(2), 0, 0, 2), 70);
-- >   var out = SelectX.ar(control, [tone1, tone2, tone3]);
-- >   Pan2.ar(out * 0.1)
-- > }).play
--
threeAlter :: UGen
threeAlter = out 0 $ pan2 (o * 0.1) 0 1
  where
    o = selectX ctrl (sinOsc ar (mce [723, 932, 1012]) 0)
    ctrl = lpf (stepper (impulse kr 2 0.5) 0 0 2 1 0) 70

-- | Same as threeAlter, with using demand ugens.
--
-- > Ndef(\alarm, {
-- >   var freq, out;
-- >   freq = Duty.kr(0.5, 0, Dseq([723, 932, 1012], inf));
-- >   freq = LPF.kr(freq, 70);
-- >   out = SinOsc.ar(freq);
-- >   Pan2.ar(out * 0.1)
-- > }).play
--
threeDemand :: UGen
threeDemand = out 0 $ pan2 (o * 0.1) 0 1
  where
    o = sinOsc ar freq 0
    freq = lpf freq' 70
    freq' = duty kr 0.5 0 DoNothing (dseq 'a' dinf (mce [723, 932, 1012]))

-- | Choice of timbral settings
--
-- > Ndef(\alarm, {
-- >                       var freq, out, operations;
-- >                       freq = Duty.kr(0.05, 0, Dseq([723, 932, 1012], inf));
-- >                       freq = LPF.kr(freq, 70);
-- >                       out = SinOsc.ar(freq);
-- >                       operations = [out, (out * pi).sin, (out * pi).cos, ((out+0.25) * pi).cos];
-- >                       out = Select.ar(MouseX.kr(0,4).poll, operations);
-- >                       Pan2.ar(out * 0.1)
-- > }).play
--
chooseTimber :: UGen
chooseTimber = out 0 $ pan2 (o * 0.1) 0 1
  where
    o = select (mouseX kr 0 4 Linear 0) operations
    operations = mce [o', sin (o' * pi), cos (o' * pi), cos ((o'+0.25)*pi)]
    o' = sinOsc ar freq 0
    freq = lpf freq' 70
    freq' = duty kr 0.05 0 DoNothing (dseq 'a' dinf $ mce [723, 932, 1012])

-- | UGen for sent to server as synthdef. Capable of wide variety of sequence.
--
-- Send synthdef with:
--
-- > > audit "dsaf_multialarm" dsaf_multialarm
--
-- > synthdef(\dsaf_multialarm, {
-- >   |length=0.05, freqs=#[600,800,600,800], timbre=1, repeats=inf|
-- >   var freq, out, operations;
-- >   freq = Duty.ar(length, 0, Dseq(freqs, repeats), doneAction: 2);
-- >   freq = LPF.ar(freq, 70);
-- >   out = LeakDC.ar(SinOsc.ar(freq));
-- >   out = Select.ar(timbre, [out, (out * pi).sin, (out * pi).cos, ((out+0.25) * pi).cos]);
-- >   // NOTE: when writing a synthdef always remember the Out ugen!
-- >   // (Handy shortcuts like Ndef and {}.play often add Out on your behalf)
-- >   Out.ar(0, Pan2.ar(out * 0.1))
-- > }).memStore;
--
dsaf_multialarm :: UGen
dsaf_multialarm = out 0 $ pan2 (o * 0.1) 0 1
  where
    o = select timbre (mce [o', sin (o'* pi), cos (o'*pi), cos((o'+0.25)*pi)])
    o' = leakDC (sinOsc ar freq 0) 0.995
    freq = lpf freq' 70
    freq' = duty ar len 0 DoNothing (dseq 'a' rep $ freqC)
    freqC = mce [control kr "freq1" 600
                ,control kr "freq2" 800
                ,control kr "freq3" 600
                ,control kr "freq4" 800]
    len = control kr "length" 0.05
    timbre = control kr "timbre" 1
    rep = control kr "repeats" 9e9

-- | Pass the nodeId of dsaf_multialarm.
--
-- > Synth(\dsaf_multialarm, [\length, 0.1, \freqs, [349, 0, 349, 0], \timbre, 1, \repeats, 1]);
--
happyBlips :: Int -> IO ()
happyBlips n = setAlarm 0.1 (349,0,349,0) 1 1 n

-- |
-- > Synth(\dsaf_multialarm, [\length, 0.1, \freqs, [238, 0, 317, 0], \timbre, 2, \repeats, 1]);
affirmative :: Int -> IO ()
affirmative n = setAlarm 0.1 (238,0,317,0) 2 1 n

-- |
-- > Synth(\dsaf_multialarm, [\length, 0.02, \freqs, [300, 125, 0, 0], \timbre, 2, \repeats, 10]);
activate :: Int -> IO ()
activate n = setAlarm 0.02 (300,125,0,0) 2 10 n

-- |
-- > Synth(\dsaf_multialarm, [\length, 0.03, \freqs, [360, 238, 174, 158], \timbre, 1]);
invaders :: Int -> IO ()
invaders n = setAlarm 0.03 (360,238,174,158) 1 9e9 n

-- |
-- > Synth(\dsaf_multialarm, [\length, 0.05, \freqs, [2000, 2010, 2000, 2010], \timbre, 1, \repeats, 6]);
information :: Int -> IO ()
information n = setAlarm 0.05 (2000,2010,2000,2010) 1 6 n

-- |
-- > Synth(\dsaf_multialarm, [\length, 0.15, \freqs, [619, 571, 365, 206], \timbre, 1, \repeats, 2]);
messageAlert :: Int -> IO ()
messageAlert n = setAlarm 0.15 (619,571,365,206) 1 2 n

-- |
-- > Synth(\dsaf_multialarm, [\length, 0.15, \freqs, [365, 571, 619, 206], \timbre, 3, \repeats, 1]);
finished :: Int -> IO ()
finished n = setAlarm 0.15 (365, 571, 619, 206) 3 1 n

-- |
-- > Synth(\dsaf_multialarm, [\length, 0.01, \freqs, [1000, 0, 1000, 0], \timbre, 3, \repeats, 30]);
errorCode :: Int -> IO ()
errorCode = setAlarm 0.01 (1000,0,1000,0) 3 30

-- | Helper for sending @n_set@ message to dsaf_multialarm node.
setAlarm :: Double                           -- ^ length
         -> (Double, Double, Double, Double) -- ^ freqs
         -> Double                           -- ^ timbre
         -> Double                           -- ^ repeats
         -> Int                              -- ^ nodeId
         -> IO ()
setAlarm l (f1,f2,f3,f4) t r n = withSC3 $ \fd ->
  send fd $ n_set n [ ("length", l)
                    , ("freq1", f1)
                    , ("freq2", f2)
                    , ("freq3", f3)
                    , ("freq4", f4)
                    , ("timbre", t)
                    , ("repeats", r) ]