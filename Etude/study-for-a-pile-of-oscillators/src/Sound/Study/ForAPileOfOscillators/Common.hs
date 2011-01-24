------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Common codes used by study of a pile of oscillators.
--
module Sound.Study.ForAPileOfOscillators.Common
  (
    -- * Actions
    showGUI
  , updateSynthdefs

    -- * UGens
  , aosc
  , ac1
  , fc1
  , pc1
  , smaster

    -- * Function to make UGen
  , mkEnv
  , BreakPoints

    -- * Node ids and bus ids
  , oscIds
  , aBusses
  , fBusses
  , pBusses
  , ampBus
  , freqBus
  , panBus

    -- * Synth nodes
  , oscs
  , afpDefault

    -- * GUI data
  , hints

    -- * Value referred from elsewhere
  , numOsc
  ) where

import Data.List (zipWith4)
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

-- | Show GUI.
showGUI :: (Transport t) => t -> IO ()
showGUI fd = do
  treeToGui (Group 0 afpDefault) hints fd

-- | Write synthdefs and reload them.
--
-- Note that, ac1, fc1, and pc1 synthdefs are too large to send in a single
-- UDP connection. From this reason, synthdefs are written to file once and
-- then reloaded.
--
updateSynthdefs :: (Transport t) => t -> IO OSC
updateSynthdefs fd = do
  mapM_ (\(n,u) -> writeSynthdef n u)
    [("aosc",aosc)
    ,("ac1",ac1)
    ,("fc1",fc1)
    ,("pc1",pc1)]
  reloadSynthdef fd

-- | Simple single sine oscillator.
aosc :: UGen
aosc = out 0 (pan2 sig pan 1)
  where
    sig = sinOsc ar freq 0 * (lag2 amp 2e-3)
    pan = ctrl "pan" 0
    amp = ctrl "amp" 0.3
    freq = ctrl "freq" 440

-- | Controller for amplitude of oscillators.
ac1 :: UGen
ac1 = mrg outs
  where
    outs = zipWith zf aBusses aBusses
    zf o u = out (fromIntegral o)
             (delayN (sig u) 1 (del * tRand u 1e-3 800e-3 t_trig))
    sig j = ((lin j * (1-mx)) + (lfn j * mx)) * val j
    lin j = envGen kr (linTrig j) 1 0 edur' DoNothing $
            env [1e-9,1e-9,1,1e-9] [0,atk,rel] [EnvNum curve] (-1) 0
    atk = 1 - edgey
    rel = edgey
    edur' = linExp edur 1e-9 1 1e-4 2
    linTrig j = coinGate 'c' chaos (dust j kr dfreq) +
                coinGate 'c' (1-chaos) (impulse kr dfreq 0)
    lfn j = lfdNoise3 j kr (linExp (nfreq+1e-9) (1e-9) 1 (1/64) 64)
    dfreq = cubed (clip2 (dmod * tmul) tmul) + toffset
    dmod = lfdNoise3 'm' kr tfreq * 0.5 + 0.5
    val i = tRand i vMin vMax t_trig
    vMin = vc - (vc * vd)
    vMax = vc + (vc * vd)
    vc = ctrl "vc" 3e-2
    vd = ctrl "vd" 0.5
    mx = ctrl "mix" 0
    nfreq = ctrl "nfreq" 1
    edgey = ctrl "edgey" 1
    edur = ctrl "edur" 300e-3
    del = ctrl "del" 1
    curve = ctrl "crv" 1
    chaos = ctrl "chaos" 0.5
    tfreq = ctrl "tfreq" 0.125
    tmul = ctrl "tmul" 5.5
    toffset = ctrl "tos" 0
    t_trig = ctrl "t_trig" 1

-- | Controller for frequency of oscillators.
fc1 :: UGen
fc1 = mrg outs
  where
    outs = zipWith out (map fromIntegral fBusses) sigs
    sigs = map f fBusses
    f i = (noise i * mx) + ((pitched' i + (nfreq i * vib)) * (1-mx))
    noise j = linExp (lfdNoise3 j kr (nfreq j) * 0.5 + 0.5 + 1e-9)
              1e-9 1 vMin vMax
              * noiseC
    pitched' j = clip (pitched j) 0 vMax
    pitched j = select (tiRand j 0 (fromIntegral $ length partials - 1) t_trig)
                (mce $ pitches j)
    pitches j = zipWith (\a b -> midiCPS (a + b)) (repeat ptc) partials
    partials = take (length fBusses) $
               zipWith (\f c -> f * 12 + c)
               (concatMap (replicate 4) [0,1..12]) (cycle overtones)
    overtones = [-12,-10,-7,-5,0,2,5,7,12]
    val j = tExpRand j vMin vMax t_trig
    vMin = (vc - (vc * vd)) + 1e-9
    vMax = vc + (vc * vd)
    nfreq j = tExpRand j fMin fMax t_trig
    fMin = fc - (fc * fd)
    fMax = fc + (fc * fd)
    vc = ctrl "vc" 8000
    vd = ctrl "vd" (1-(1e-9))
    fc = ctrl "fc" 2
    fd = ctrl "fd" 0.5
    mx = ctrl "mix" 1
    ptc = ctrl "ptc" 0
    vib = ctrl "vib" 0
    noiseC = ctrl "noise" 1
    t_trig = ctrl "t_trig" 1

-- | Controller for pan of oscillators.
pc1 :: UGen
pc1 = mrg outs
  where
    outs = zipWith out (map fromIntegral pBusses) sigs
    sigs = map f ['a'..]
    f i = clip ((noise i + sinu i + e + offset) * val i) (-1) 1
    noise j = (lfdNoise3 j kr (freq j) * 0.5 + 0.5) * noiseC
    sinu j = (sinOsc kr (freq j) 0 * 0.5 + 0.5) * sinC
    val j = vc + tRand j (vc-(vd/2)) (vc+(vd/2)) t_trig
    freq j = fc + tRand j (fc-(fd/2)) (fc+(fd/2)) t_trig
    e = linen t_trig atk lev rel DoNothing
    lev = tRand 'l' (-1) 1 t_trig
    atk = tRand 'a' 1e-3 1200e-3 t_trig
    rel = tRand 'r' 1e-3 1200e-3 t_trig
    offset = ctrl "offset" 0
    vc = ctrl "vc" 1
    vd = ctrl "vd" 0.5
    fc = ctrl "fc" 2
    fd = ctrl "fd" 0.999
    noiseC = ctrl "noise" 0
    tickC = ctrl "tick" 0
    sinC = ctrl "sin" 0
    t_trig = ctrl "t_trig" 1


-- | BreakPoints for (time, value)
type BreakPoints = [(UGen,UGen)]

-- | Helper for making envelope.
mkEnv :: EnvCurve      -- ^ Curve shape
      -> UGen          -- ^ Level scale
      -> UGen          -- ^ Time scale
      -> [(UGen,UGen)] -- ^ Break points of (time,value)
      -> UGen
mkEnv curve ls ts bps =
  envGen kr (ctrl "t_trig" 1) ls 0 ts DoNothing $ envCoord bps 1 1 curve

-- | Simple mixer
smaster :: UGen
smaster = replaceOut 0 (sig * amp)
  where
    sig = freeVerb2 (in' 1 ar 0) (in' 1 ar 1) rmix rroom rdamp
    amp = ctrl "amp" 1
    rmix = ctrl "rmix" 0.5
    rroom = ctrl "rroom" 0.5
    rdamp = ctrl "rdamp" 0.5

-- | Number of oscillators
numOsc :: Num a => a
numOsc = 256

-- | Node ids for oscillators
oscIds :: [Int]
oscIds = [20001.. 20001+numOsc]

-- | Bus ids for mapping amplitudes.
aBusses :: [Int]
aBusses = [1001..1001+numOsc]

-- | Bus ids for mapping frequencies.
fBusses :: [Int]
fBusses = [2001..2001+numOsc]

-- | Bus ids for mapping pans.
pBusses :: [Int]
pBusses = [3001..3001+numOsc]

-- | Control bus used for amp of the oscillator
ampBus :: (Num a)
       => Int  -- ^ Node id of oscillator synth node
       -> a
ampBus k = 1001 + (fromIntegral k - 20001)

-- | Control bus used for freq of the oscillator
freqBus :: (Num a)
        => Int     -- ^ Node id of oscillator synth node
        -> a
freqBus k = 2001 + (fromIntegral k - 20001)

-- | Control bus used for pan of the oscillator
panBus :: (Num a)
       => Int     -- ^ Node id of oscillator synth node
       -> a
panBus k = 3001 + (fromIntegral k - 20001)

-- | Synth nodes for ac1, fc1, and pc1, with default params taken from ugen.
afpDefault :: [SCNode]
afpDefault =
  [Synth 1001 "ac1" (f ac1)
  ,Synth 1002 "fc1" (f fc1)
  ,Synth 1003 "pc1" (f pc1)]
  where
   f = map (\(NodeK _ _ n v _) -> n:=v) . controls . synth

-- | Oscillator synth nodes.
oscs :: [SCNode]
oscs = o oscIds aBusses fBusses pBusses
  where
    o = zipWith4 (\x a f p-> Synth x "aosc" ["amp":<-a,"freq":<-f,"pan":<-p])

-- | Hints for showing ac1, fc1, and pc1.
hints :: Hints
hints = M.fromList
 [("ac1",
   [ParamRange "vc" 0 1e-1
   ,ParamRange "vd" 0 1
   ,ParamRange "mix" 0 1
   ,ParamRange "nfreq" 0 1
   ,ParamRange "edgey" 0 1
   ,ParamRange "edur" 0 1
   ,ParamRange "crv" (-18) 18
   ,ParamRange "del" 0 1
   ,ParamRange "chaos" 0 1
   ,ParamRange "tfreq" 0 8
   ,ParamRange "tmul" 0 8
   ,ParamRange "tos" 0 8
   ,ParamRange "t_trig" 0 1])
 ,("fc1",
   [ParamRange "vc" 0 8000
   ,ParamRange "vd" 0 (1-1e-9)
   ,ParamRange "mix" 0 1
   ,ParamRange "fc" 0 3
   ,ParamRange "fd" 0 1
   ,ParamRange "ptc" 0 12
   ,ParamRange "vib" 0 10
   ,ParamRange "noise" 0 1
   ,ParamRange "t_trig" 0 1])
 ,("pc1",
   [ParamRange "vc" (-1) 1
   ,ParamRange "vd" 0 1
   ,ParamRange "fc" 0 3
   ,ParamRange "fd" 0 1
   ,ParamRange "t_trig" 0 1])]
