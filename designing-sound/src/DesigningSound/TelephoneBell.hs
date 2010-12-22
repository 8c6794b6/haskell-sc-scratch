------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Telephone bells.
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Telephone_bell>
--
-- /Example/:
--
-- > > audition metallic
-- > > audition striker
-- > > audit "phoneBell1" phoneBell1
-- > > snew "phoneBell1" [("freq", 500)]
-- > > audition $ out 0 $ pan2 (phonecase2 (impulse ar 1 0.5)) 0 1
--
-- /Puting it all together example/:
--
-- > > drecv "dsaf_phonecase1" phonecase1
-- > > drecv "dsaf_phonebell2" dsaf_phonebell2
-- > > addBells
-- > > addBakeLite
-- > > turnOnAndOff
--
module DesigningSound.TelephoneBell where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID

import DesigningSound.Util

-- | Simple metallic resonance with three pitches
--
-- > {
-- >   var son;
-- >   son = Klank.ar(`[
-- >     [521, 732, 934],  // freqs
-- >     [0.7, 0.45, 0.25],// amps
-- >     [0.8, 0.8, 0.8]   // ring times
-- >   ]
-- >   , Impulse.ar(1));
-- >   Out.ar(0, Pan2.ar(son * 0.2));
-- > }.play
--
metallic :: UGen
metallic = out 0 $ pan2 (son * 0.2) 0 1
  where
    son = klank (impulse ar 1 0) 1 0 1 spec
    spec = klankSpec [521, 732, 934] [0.7, 0.45, 0.25] [0.8, 0.8, 0.8]

-- | A striker sound
--
-- > x = {
-- >    var son = WhiteNoise.ar(XLine.ar(1, 0.000001, 0.01, doneAction: 2)) * 0.1;
-- >    Out.ar(0, Pan2.ar(son));
-- > }.play
--
striker :: UGen
striker = out 0 $ pan2 (son * 0.2) 0 1
  where
    son = whiteNoise 'a' ar * (xLine ar 1 0.000001 0.01 RemoveSynth)

-- | Many resonators to create the various modes of the bell
--
-- > SynthDef(\dsaf_phonebell1, { |freq=465, strength=1, decay=3|
-- >   var son;
-- >   son = Klank.ar(`[
-- >     // frequency ratios
-- >     [0.501, 1, 0.7,   2.002, 3, 9.6,   2.49, 11, 2.571,  3.05, 6.242, 12.49, 13, 16, 24],
-- >     // amps
-- >     [0.002,0.02,0.001, 0.008,0.02,0.004, 0.02,0.04,0.02, 0.005,0.05,0.05, 0.02, 0.03, 0.04],
-- >     // ring times - "stutter" duplicates each entry threefold
-- >     [1.2, 0.9, 0.25, 0.14, 0.07].stutter(3)
-- >   ]
-- >   , Impulse.ar(1), freq, 0, decay);
-- >   Out.ar(0, Pan2.ar(son));
-- > }).memStore
-- >
-- > x = Synth(\dsaf_phonebell1, [\freq, 500]);
-- >
--
phoneBell1 :: UGen
phoneBell1 = out 0 $ pan2 son 0 1
  where
    son = klank (impulse ar 1 1) freq 0 dcy spec
    freq = control kr "freq" 465
    dcy = control kr "decay" 3
    spec = klankSpec
     [ 0.501, 1, 0.7, 2.002, 3, 9.6, 2.49, 11,
       2.571, 3.05, 6.242, 12.49, 13, 16, 24 ]
     [ 0.002, 0.02, 0.001, 0.008, 0.02, 0.004, 0.02, 0.04,
       0.02, 0.005, 0.05, 0.05, 0.02, 0.03, 0.04]
     (concat $ replicate 3 [1.2, 0.9, 0.25, 0.14, 0.07])

-- | Resonant effects of bakelite phone casing
--
-- > (
-- > SynthDef(\dsaf_phonecase1, { |in=0, out=0, mix=0|
-- >   var casein = In.ar(in, 2);
-- >
-- >   var delayA = CombC.ar(casein, 0.00077, 0.00077, 0.1);
-- >   var delayB = CombC.ar(delayA, 0.00088, 0.00088, 0.1);
-- >   var bands = BPF.ar(delayB, [1243, 287, 431], 1/12).sum;
-- >   var son = bands.clip2(0.3);
-- >
-- >   ReplaceOut.ar(out, XFade2.ar(casein, son, mix))
-- >
-- > }).memStore;
-- > )
-- > y = Synth(\dsaf_phonecase1, target: x, addAction: \addAfter);
--
phonecase1 :: UGen
phonecase1 = replaceOut o (xFade2 casein son m 1)
  where
    o = control kr "out" 0
    m = control kr "mix" 0
    i = control kr "in" 0
    casein = in' 2 ar i
    son = clip2 bands 0.3
    bands = mix $ bpf delayB (mce [1243, 287, 431]) (1/12)
    delayA = combC casein 0.00077 0.00077 0.1
    delayB = combC delayA 0.00088 0.00088 0.1

-- | Functional variant of @phonecase1@
--
-- > // Notice that for this shortcut visualisation, we define the filter as a //
-- > // function taking its input as an argument, and returning its output (we don't
-- > // use In.ar or Out.ar)
-- > { |casein|
-- >   var delayA = CombC.ar(casein, 0.00077, 0.00077, 0.1);
-- >   var delayB = CombC.ar(delayA, 0.00088, 0.00088, 0.1);
-- >   var bands = BPF.ar(delayB, [1243, 287, 431], 1/12).sum;
-- >   var son = bands.clip2(0.3);
-- >
-- >   // Mouse to the LEFT means flat filter (no change), to the RIGHT means
-- >   // full bakelite
-- >   XFade2.ar(casein, son, MouseX.kr(-1,1));
-- >
-- > }.scopeResponse
--
phonecase2 :: UGen -> UGen
phonecase2 ug = xFade2 ug son (mouseX kr (-1) 1 Linear 1) 1
  where
    son = clip2 bands 0.3
    bands = mix $ bpf delayB (mce [1243, 287, 431]) (1/12)
    delayB = combC delayA 0.00088 0.00088 0.1
    delayA = combC ug 0.00077 0.00077 0.1

-- | Synthdef for controlling on and off.
--
-- > SynthDef(\dsaf_phonebell2, { |gate=1, freq=465, strength=1, decay=3, amp=1|
-- >   var trigs, striker, son;
-- >   trigs = Impulse.ar(14) * gate;
-- >   striker = WhiteNoise.ar(EnvGen.ar(Env.perc(0.0000001, 0.01), trigs));
-- >   son = Klank.ar(`[
-- >     // frequency ratios
-- >     [0.501, 1, 0.7,   2.002, 3, 9.6,   2.49, 11, 2.571,  3.05, 6.242, 12.49, 13, 16, 24],
-- >     // amps
-- >     [0.002,0.02,0.001, 0.008,0.02,0.004, 0.02,0.04,0.02, 0.005,0.05,0.05, 0.02, 0.03, 0.04],
-- >     // ring times - "stutter" duplicates each entry threefold
-- >     [1.2, 0.9, 0.25, 0.14, 0.07].stutter(3)
-- >   ]
-- >   , striker, freq, 0, decay);
-- >   Out.ar(0, Pan2.ar(son * amp));
-- > }).memStore
-- > )
--
dsaf_phonebell2 :: UGen
dsaf_phonebell2 = out 0 $ pan2 (son * amp) 0 1
  where
    son = klank strike freq 0 dcy spec
    strike = whiteNoise 'a' ar * envGen ar trigs 1 0 1 DoNothing shape
    trigs = impulse ar 14 1e-2 * gt
    shape = envPerc 1e-7 1e-2
    spec = klankSpec
           [0.501, 1, 0.7, 2.002, 3, 9.6, 2.49, 11, 2.571, 3.05,
            6.242, 12.49, 13, 16, 24]
           [0.002, 0.02, 0.001, 0.008, 0.02, 0.004, 0.02, 0.04, 0.02, 0.005,
            0.05, 0.05, 0.02, 0.03, 0.04]
           (concat $ replicate 3 [1.2, 0.9, 0.25, 0.14, 0.07])
    amp = control kr "amp" 1
    gt = control kr "gate" 1
    freq = control kr "freq" 465
    dcy = control kr "decay" 3

-- | Playing the dssf_phonebell2 synthdef.
--
-- > // We could launch the patch all at once, but let's do it bit-by-bit so we
-- > // understand what's going on
-- >
-- > // Here we start the phone bells constantly ringing. We put them in a
-- > // group for convenience
-- > ~bellgroup = Group.new(s);
-- > ~bell1 = Synth(\dsaf_phonebell2, [\freq, 650], ~bellgroup);
-- > ~bell2 = Synth(\dsaf_phonebell2, [\freq, 653], ~bellgroup);
-- >
-- > // Now we add the bakelite
-- > y = Synth(\dsaf_phonecase1, [\mix, -0.65], target: ~bellgroup, addAction: \addAfter);
-- >
-- > // OK, shush for now
-- > ~bellgroup.set(\gate, 0);
-- >
-- > // Now let's turn them on and off in a telephone-like pattern.
-- > // This could be done using a synth, but let's use a (client-side) pattern:
-- > p = Pbind(\type, \set, \id, ~bellgroup.nodeID, \args, [\gate], \gate, Pseq([1,0], inf), \dur, 2).play
-- > p.stop
--
addBells :: IO ()
addBells = withSC3 $ \fd -> do
  async fd $ d_recv $ synthdef "dsaf_phonebell2" dsaf_phonebell2
  send fd $ g_new [(bellGroupId, AddToTail, 1)]
  let newBell f =
        send fd $ s_new "dsaf_phonebell2" (-1) AddToTail bellGroupId [("freq", f)]
  mapM_ newBell [650, 653]

-- | Add bakelite synth after bells.
addBakeLite :: IO Int
addBakeLite = withSC3 $ \fd -> do
  async fd $ d_recv $ synthdef "dsaf_phonecase1" phonecase1
  send fd $ s_new "dsaf_phonecase1" (-1) AddAfter bellGroupId [("mix", -0.65)]
  send fd $ s_get (-1) []
  (Message _ (Int nid:_)) <- wait fd "/n_set"
  return nid

-- | Turn bells on and off repeatedly.
turnOnAndOff :: IO ()
turnOnAndOff = forever (go 1 >> go 0)
  where
    go v = nset bellGroupId [("gate", v)] >> threadDelay (2 * 1000 * 1000)

-- | Group id for bells.
bellGroupId :: Int
bellGroupId = 100
