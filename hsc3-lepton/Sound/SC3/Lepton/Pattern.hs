------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : unstable
-- Portability : non-portable (Rank2Types, FlexibleContexts)
--
-- Pattern DSL.
--
-- Concept was inspired from sclang and Sound.SC3.Pattern in
-- hsc3-lang. Implementation was inspired from Oleg Kiselyov's TTF
-- (<http://okmij.org/ftp/tagless-final/>).
--
-- There is no intention to translate whole set of Pattern
-- classes found in sclang.
--
module Sound.SC3.Lepton.Pattern
  ( -- * Create, view, and run
    -- $example_intro

    -- * Making sequence of pitch
    -- $example_spe

    -- * Grouping and traversing
    -- $example_combine

    -- * Low level tweaks
    -- $example_low_level

    -- * Sequencing with server
    -- $example_server

    module All
  ) where

import Sound.SC3.Lepton.Pattern.Expression as All
import Sound.SC3.Lepton.Pattern.Client as All
import Sound.SC3.Lepton.Pattern.Interpreter as All
import Sound.SC3.Lepton.Pattern.Play as All
import Sound.SC3.Lepton.Pattern.ToOSC as All

{-$example_intro

Making pattern in ghci:

> > :set -XNoMonomorphismRestriction
> > let p1 = pchoose (pval 3) [pval 10, plist [1..5]]

Viewing the pattern:

> > showP p1
> "pchoose (pval 3) [pval 10,plist [1,2,3,4,5]]"

And running it:

> > runPIO p1 -- try several times
> [10,1,2,3,4,5,10]

The type of this pattern is:

> > :t p1
> p1 :: (Pval p, Num t, Enum t, Plist p, Pchoose p) => p t

-}

{-$example_spe

Translation of \"/Understanding Streams, Patterns, and Events - Part 3/\"
example from supercollider help file.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Control.Concurrent (threadDelay)
> import System.Random (newStdGen, randomRs)
>
> import Sound.OpenSoundControl
> import Sound.SC3
> import Sound.SC3.ID
>
> import Sound.SC3.Lepton.Pattern
>
> main :: IO ()
> main = withSC3 go
>
> -- | Load synthdef and play the pattern.
> go :: (Transport t) => t -> IO ()
> go fd = do
>   async fd . d_recv . synthdef "speSynth" =<< speSynth
>   foldPIO_ f () pspe
>   where
>     f _ v = do
>       send fd $ s_new "speSynth" (-1) AddToTail 1 [("freq",midiCPS v)]
>       threadDelay (floor $ 0.13 * 1e6)
>
> -- | Synthdef for spe example.
> speSynth :: IO UGen
> speSynth = do
>   dl <- randomRs (0,0.05) `fmap` newStdGen
>   dr <- randomRs (0,0.05) `fmap` newStdGen
>   return $ out 0 $ mkSig dl dr
>   where
>     mkSig dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
>     v = rlpf (lfSaw AR freq 0 * evl) nz 0.1
>     f a b = allpassN b 0.05 a 4
>     evl = envGen KR 1 1 0 1 RemoveSynth shp * 0.3
>     shp = envPerc 10e-3 1
>     nz = midiCPS (lfNoise1 'z' KR 1 * 36 + 110)
>     freq = control KR "freq" 440
>
> -- Pattern used for pitches.
> pspe =
>   pcycle
>     [pchoose 1
>        [pempty, plist [24,31,36,43,48,55]]
>     ,pseq (prange 2 5)
>        [60, pchoose 1 [63,65], 67, pchoose 1 [70,72,74]]
>     ,pchoose (prange 3 9)
>        [74,75,77,79,81]]

Below is the original pattern written in sclang:

> freqStream = Pseq([
>    Prand([
>       nil, // a nil item reached in a pattern causes it to end
>       Pseq(#[24, 31, 36, 43, 48, 55]);
>    ]),
>    Pseq([ 60, Prand(#[63, 65]), 67, Prand(#[70, 72, 74]) ], { rrand(2, 5) }),
>    Prand(#[74, 75, 77, 79, 81], { rrand(3, 9) })
> ], inf).asStream.midicps;

-}

{-$example_combine

Playing patterns with play function from 'Audible' class.

> import Control.Concurrent (threadDelay)
> import Data.Map ((!))
> import Data.Traversable (sequenceA)
> import qualified Data.Map as M
>
> import Sound.SC3
> import Sound.SC3.Lepton
>
> main :: IO ()
> main = withSC3 goBuzz
>
> -- | Play the pattern.
> goBuzz :: (Transport t) => t -> IO ()
> goBuzz fd = do
>   async fd $ d_recv $ synthdef "buzz" buzz
>   play fd (madjust "dur" (\t -> t*60/160) pBuzz :: R (ToOSC Double))
>
> -- | UGen for buzz.
> buzz :: UGen
> buzz = out 0 $ pan2 sig pan 1
>   where
>     sig = sinOsc AR freq 0 * amp * e
>     e = linen tr 5e-3 1 (10e-3+(220/freq)) RemoveSynth ^ 2
>     amp = control KR "amp" 0.3
>     freq = control KR "freq" 440
>     pan = control KR "pan" 0
>     tr = tr_control "t_trig" 1
>
> -- Pattern for amp, dur, freq, and pan.
> pBuzz = snew "buzz" Nothing AddToTail 1
>   [("amp", pcycle [0.3, 0.1,  0.1,   0.3,  0.1,  0.1,  0.1])
>   ,("dur", pcycle [1,   0.55, 0.45,  0.54, 0.46, 0.53, 0.47])
>   ,("freq", fmap midiCPS $
>             pcycle [48, pchoose 13 cm, 53, pchoose 13 fm
>                    ,48, pchoose 13 cm, 43, pchoose 13 g7
>                    ,48, pchoose 13 cm, 53, pchoose 13 fm
>                    ,50, pchoose 6 fm, 43, pchoose 6 g7
>                    ,48, pchoose 6 cm, 55, pchoose 6 cm])
>   ,("pan", pcycle [plist [-1,-0.9..1], plist [1,0.9..(-1)]])]
>   where
>     cm = [55, 67,72,75,79,84,87]
>     fm = [60, 68,72,77,80,84,89]
>     g7 = [50, 67,71,74,77,79,83]

-}

{-$example_low_level

Directly writing function for R:

> > runPIO $ pseq 3 [1,2,R $ \_ -> [999,1024]]
> [1,2,999,1024,1,2,999,1024,1,2,999,1024]

For S:

> > showP $ prand (S $ \_ -> "foo bar buzz") [1..5]
> "prand (foo bar buzz) [pval 1,pval 2,pval 3,pval 4,pval 5]"

-}

{-$example_server

This package contains an executable `leptseq`, to fork and play patterns. To
run the server, invoke below from shell:

> $ leptseq

and server will run with default port. To see help:

> $ leptseq --help

Will show breif usage.

Below is a sample module inspired from  /on-the-fly synchronization (concurrent)/
example in chuck, by Perry and Ge, from:
<http://chuck.cs.princeton.edu/doc/examples/> .

> import System.FilePath
>
> import Sound.OpenSoundControl
> import Sound.SC3
> import Sound.SC3.ID
> import Sound.SC3.Lepton
> import Sound.SC3.Lepton.Pattern.Client
>
> ------------------------------------------------------------------------------
> -- Synth defs
>
> otfperc = out ("out"@@0) sig where
>   sig = pan2 (pb * ("amp"@@0.3)) ("pan"@@0) 1
>   pb = playBuf 1 ("bnum"@@0) 1 1 0 NoLoop RemoveSynth
>
> otfsine = out ("out"@@0) sig where
>   sig = pan2 (sinOsc AR ("freq"@@0) 0 * e) ("pan"@@0) 1
>   e = envGen KR 1 ("amp"@@0) 0 ("dur"@@1) RemoveSynth $
>       env [0,1,1,0] [1e-3,998e-3,1e-3] [EnvLin] 0 0
>
> otfrev1 = replaceOut ("out"@@0) sig where
>   sig = freeVerb2 inl inr 0.5 0.5 0.5
>   inl = (in' 1 AR ("inl"@@0))
>   inr = (in' 1 AR ("inr"@@1))
>
> otfrev2 = out ("out"@@0) sig where
>   sig = ins * m + foldr f ins [1..8::Int] * (1-m)
>   m = "mix"@@0.6
>   ins = in' 2 AR ("in"@@0)
>   f a b = allpassC b 0.8 (rand a 1e-3 8e-2) (rand a 4e-2 4)
>
> ------------------------------------------------------------------------------
> -- Patterns
>
> kikP = psnew "otfperc" Nothing AddToHead 1
>   [("dur", pforever t)
>   ,("bnum", prepeat 0)
>   ,("pan", prepeat (-0.1))
>   ,("amp", pforever (prange 0.5 0.7))]
>
> snrP = psnew "otfperc" Nothing AddToHead 1
>   [("dur", pforever (prepeat t * prand 1 [2, plist [0.75,1.25]]))
>   ,("bnum", prepeat 1)
>   ,("pan", prepeat 0.3)]
>
> hatP = psnew "otfperc" Nothing AddToHead 1
>   [("dur", pforever (prepeat t * prand 1 [0.5, plist [0.25,0.25]]))
>   ,("bnum", prepeat 2)
>   ,("pan", prepeat (-0.3))
>   ,("amp", pforever (prange 0.3 0.4))]
>
> hatoP = psnew "otfperc" Nothing AddToHead 1
>   [("dur", pforever t)
>   ,("bnum", prepeat 3)
>   ,("pan", prepeat (-0.3))
>   ,("amp", pforever (prange 0.2 0.4))]
>
> sin1P = psnew "otfsine" Nothing AddToHead 1
>   [("dur", pforever (t * 0.25))
>   ,("out", prepeat 2)
>   ,("freq",
>     pforever $ midiCPS (21 + (prand 1 [0,12,24,36] + prand 1 [0,2,4,7,9])))
>   ,("amp", pforever 0.5)
>   ,("pan", pforever 0.1)]
>
> sin2P = psnew "otfsine" Nothing AddToHead 1
>   [("dur", pforever (t * prand 1 [0.5,0.25]))
>   ,("out", prepeat 2)
>   ,("freq",
>     pforever $ midiCPS (69 + (prand 1 [0,12,24,36] + prand 1 [0,2,4,7,9])))
>   ,("amp", pforever 0.2)
>   ,("pan", pforever (-0.2))]
>
> t = 0.5
>
> sin1N = 0xbaca
>
> sin2N = 0xcaba
>
> ------------------------------------------------------------------------------
> -- Sends synthdefs, allocate buffers, add new synth nodes.
>
> setup'otf = withSC3 $ \fd -> do
>   reset fd
>   async fd $ bundle immediately $
>     (map (d_recv . uncurry synthdef))
>      [("otfperc",otfperc),("otfsine", otfsine)
>      ,("otfrev1", otfrev1),("otfrev2",otfrev2)] ++
>     (zipWith (\i file -> b_allocRead i (soundsDir </> file) 0 0)
>      [0..]
>      ["kick.wav", "snare-hop.wav", "hihat.wav", "hihat-open.wav"])
>   patchNode otfNodes fd
>
> soundsDir = "souncs/examples/data" -- modify this appripriately
>
> otfNodes =
>   g 0
>     [g 1 []
>     ,g 2
>       [s 1000 "otfrev1" []
>       ,s 1001 "otfrev2" ["in":=2,"mix":=0.5]]]
>   where
>     g = Group; s = Synth
>
> ------------------------------------------------------------------------------
> -- Actions for patterns
>
> addKik = addPat 0 "kik" kikP
> addHat = addPat 0 "hat" hatP
> addHato = addPat 0 "hat-open" hatoP
> addSnr = addPat t "snr" snrP
> addSin1 = addPat 0 "sin-lo" sin1P
> addSin2 = addPat 0 "sin-hi" sin2P

After loading above module in ghci, try:

> > setup'otf
> > addKik
> > addHat >> addHato
> > addSin1
> > addSnr
> > addSin2

To stop:

> > resetAll

-}
