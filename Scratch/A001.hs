------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with additive synth.
--
module A001 where

import Control.Monad (forever)
import Data.List (zipWith4)
import System.Random (newStdGen, randomRIO, randomRs)
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

main :: IO ()
main = withSC3 $ \fd -> do
  setup fd
  treeToGui (Group 0 [Group 1 afp]) hints fd

setup :: (Transport t) => t -> IO ()
setup fd = do
  mapM_ (\(n,u) -> writeSynthdef n u)
    [("aosc",aosc)
    ,("bosc",bosc)
    ,("fControl",fControl)
    ,("pControl",pControl)
    ,("lin",lin)
    ,("ac1",ac1)
    ,("fc1",fc1)
    ,("metro",metro)]
  reloadSynthdef fd
  mkTree oscTree fd

-- | Main synth node tree.
oscTree :: SCTree
oscTree =
  Group 0
    [Group 1
      [Group 10 afp
      ,Group 20 oscs]]

-- | Controller synth nodes.
afp :: [SCTree]
afp = [Synth 1001 "ac1"
         ["vc":=8e-3,"vd":=5e-3,
          "mix":=0,"nfreq":=1,
          "edgey":=1,"edur":=500e-3,"crv":=(-12),
          "del":=0.01,"chaos":=0.5,
          "tfreq":=0.8,"tmul":=3,"tos":=0.6,
          "t_trig":=1]
      ,Synth 1002 "fc1"
         ["vc":=5200,"vd":=5000,"fc":=1,"fd":=0.5,
          "mix":=0,
          "ptc":=0,"vib":=0,"t_trig":=1]
      ,Synth 1003 "pControl"
         ["fc":=1,"fd":=0.5,"vc":=0,"vd":=1,"t_trig":=1]]

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
   ,ParamRange "t_trig" 0 1])
 ,("pControl",
   [ParamRange "vc" (-1) 1
   ,ParamRange "vd" 0 1
   ,ParamRange "fc" 0 3
   ,ParamRange "fd" 0 1
   ,ParamRange "t_trig" 0 1])]

-- | Oscillator synth nodes.
oscs :: [SCTree]
oscs = o allIds aBusses fBusses pBusses
  where
    o = zipWith4 (\x a f p-> Synth x "aosc" ["amp":<-a,"freq":<-f,"pan":<-p])

-- | Number of oscillators
numOsc :: Num a => a
numOsc = 64

allIds :: [Int]
allIds = [20001.. 20001+numOsc]

aBusses :: [Int]
aBusses = [1001..1001+numOsc]

fBusses :: [Int]
fBusses = [2001..2001+numOsc]

pBusses :: [Int]
pBusses = [3001..3001+numOsc]

fControl :: UGen
fControl = mkControl fBusses tExpRand

-- | Simple single sin oscillator.
aosc :: UGen
aosc = out 0 (pan2 sig pan 1)
  where
    sig = sinOsc ar freq 0 * (lag2 amp 5e-3)
    pan = ctrl "pan" 0
    amp = ctrl "amp" 0.3
    freq = ctrl "freq" 440

bosc :: UGen
bosc = out 0 $ pan2 sig pan 1
  where
    sig = mix (sinOsc ar freq 0) * amp
    freq = ctrls "freq" [100,330,440,880]
    amp = ctrl "amp" 0.1
    pan = ctrl "pan" 0

-- | Controller for amplitude of oscillators.
ac1 :: UGen
ac1 = mrg outs
  where
    outs = zipWith zf aBusses aBusses
    zf o u = out (fromIntegral o)
             (delayN (sig u) 1 (del * tRand u 1e-2 1 t_trig))
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
    tmul = ctrl "tmul" 2
    toffset = ctrl "tos" 1
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
    pitched' j = clip (pitched j) vMin vMax
    pitched j = select (tiRand j 0 (fromIntegral $ length partials - 1) t_trig)
                (mce $ pitches j)
    pitches j = zipWith (\a b -> midiCPS (a + b)) (repeat ptc) partials
    partials = take (length fBusses) $
               zipWith (\f c -> f * 12 + c)
               (concatMap (replicate 4) [0,1..12]) (cycle [0,4,7,11,14])
    val j = tExpRand j vMin vMax t_trig
    vMin = vc - (vc * vd)
    vMax = vc + (vc * vd)
    nfreq j = tExpRand j fMin fMax t_trig
    fMin = fc - (fc * fd)
    fMax = fc + (fc * fd)
    vc = ctrl "vc" 1
    vd = ctrl "vd" 0.5
    fc = ctrl "fc" 2
    fd = ctrl "fd" 0.5
    mx = ctrl "mix" 1
    ptc = ctrl "ptc" 0
    vib = ctrl "vib" 0
    noiseC = ctrl "noise" 1
    t_trig = ctrl "t_trig" 1

-- | Controller for pan of oscillators.
pControl :: UGen
pControl = mkControl pBusses tRand

mkControl :: [Int]                                  -- ^ Node ids
          -> (Char -> UGen -> UGen -> UGen -> UGen) -- ^ tRand or tExpRand
          -> UGen
mkControl bs rUGen= mrg outs
  where
    outs = zipWith out (map fromIntegral bs) sigs
    sigs = map f ['a'..]
    f i = (noise i + tick i + sinu i + e + offset) * val i
    noise j = (lfdNoise3 j kr (freq j) * 0.5 + 0.5) * noiseC
    tick j = (decay2 (impulse kr (freq j) 0) 2e-3 800e-3) * tickC
    sinu j = (sinOsc kr (freq j) 0 * 0.5 + 0.5) * sinC
    val j = vc + rUGen j (vc-(vd/2)) (vc+(vd/2)) t_trig
    freq j = fc + rUGen j (fc-(fd/2)) (fc+(fd/2)) t_trig
    e = linen t_trig atk 1 rel DoNothing
    offset = ctrl "offset" 0
    vc = ctrl "vc" 1
    vd = ctrl "vd" 0.5
    fc = ctrl "fc" 2
    fd = ctrl "fd" 1
    noiseC = ctrl "noise" 1
    tickC = ctrl "tick" 0
    sinC = ctrl "sin" 0
    atk = ctrl "atk" 5e-2
    rel = ctrl "rel" 5e-2
    t_trig = ctrl "t_trig" 1

------------------------------------------------------------------------------
--
--
-- Client side control
--
--
------------------------------------------------------------------------------

-- | UGen to send sendTrig message periodically.
metro :: UGen
metro = mrg [sendTrig tr 1 val]
  where
    tr = impulse kr bpm 0
    val = pulseCount tr rst `mod` 1024
    bpm = ctrl "bpm" 60 / 60
    rst = ctrl "reset" 0

-- | UGen to send linear value to control bus.
lin :: UGen
lin = out outBus val
  where
    outBus = ctrl "out" 0
    val = line kr str end dur RemoveSynth
    str = ctrl "str" 0
    end = ctrl "end" 1
    dur = ctrl "dur" 1

-- | Set random params
rSet :: (Transport t)
      => String
      -> Double
      -> Double
      -> t
      -> IO ()
rSet name lo hi fd = do
  vals <- newStdGen >>= return . randomRs (lo,hi)
  mkSet name allIds vals fd

-- | Set value with repeating given list.
eSet :: (Transport t)
     => String
     -> [Double]
     -> t
     -> IO ()
eSet name vs fd = mkSet name allIds (cycle vs) fd

-- | Make n_set messages.
mkSet :: (Transport t)
      => String
      -> [Int]
      -> [Double]
      -> t
      -> IO ()
mkSet name ids vs fd = do
  now <- utcr
  send fd $ Bundle (UTCr $ now + 1) $
    zipWith (\n v -> n_set n [(name,v)]) ids vs

lins :: (Transport t)
     => [Int] -> [Double] -> [Double] -> [Double] -> t -> IO ()
lins is ss es ds fd = send fd $ Bundle (NTPi 0) ms
  where
    ms = zipWith4 (\o s e d -> s_new "lin" (-1) AddToTail 10
                               [("out",fromIntegral o),("str",s)
                               ,("end",e),("dur",d)])
         is ss es ds

-- | Send a message from client to server as response of sendTrig ugen.
--
-- Try:
--
-- > > withSC3 $ asResponse responder
--
asResponse :: (Transport t) => (Double -> t -> IO ()) -> t -> IO ()
asResponse act fd = async fd (notify True) >> forever g
  where
    g = do
      Message "/tr" [Int _,Int 1,Float b] <- wait fd "/tr"
      act b fd

responder :: (Transport t) => Double -> t -> IO ()
responder n fd
  | at 0 = do
    print n
    f 1001 [("tick",1),("noise",0),("sin",0),("t_trig",1) ,("vc",30e-3)
           ,("vd",25e-3)]
    f 1002 [("sin",1),("noise",0),("t_trig",1),("vc",1200),("vd",800)]
  | at 8 = do
    f 1001 [("vc",30e-3),("vd",25e-3)]
    f 1003 [("t_trig",1)]
  | at 16 = do
    f 1001 [("vc",30e-3),("vd",25e-3)]
  | at 17 = do
    f 1002 [("sin",0),("noise",1)]
  | at 32 = do
    f 1002 [("vc",800),("vd",20)]
    f 1001 [("vc",30e-3),("vd",25e-3)]
  | at 48 = do
    f 1001 [("vc",30e-3),("vd",25e-3)]
  | at 54 = do
    f 1001 [("tick",0),("noise",1)]
  | at 60 = do
    f 1002 [("vc",4000),("vf",4000)]
  | otherwise = return ()
  where
    at k = floor n `mod` 64 == k
    f n p = send fd $ n_set n p

ws :: (Transport t) => Double -> t -> IO ()
ws d fd = w1 d fd >> w2 d fd >> w3 d fd

-- | Worker for amp.
w1 :: (Transport t) => Double -> t -> IO ()
w1 n fd
  | at 0 = hit (const True) >> print n --even
  | at 1 = hit odd
  | at 2 = hit even
  | at 3 = hit even
  | at 4 = do
    as <- randomRIOs (1e-3,3e-2)
    ae <- randomRIOs (1e-3,3e-2)
    lins aBusses as ae (repeat 1) fd
  | at 5 = hit even
  | at 6 = hit odd
  | at 7 = hit (const True)
  | at 8 = hit even
  | at 9 = hit odd
  | at 10 = do
    let as = repeat 0
    ae <- randomRIOs (1e-3,3e-2)
    ds <- randomRIOs (5e-3,1e-2)
    lins aBusses as ae ds fd
  | at 11 = hit odd
  | at 12 = do
    let ds = repeat 5e-3
    let as = repeat 0
    ae <- randomRIOs (1e-3,3e-2)
    lins (filter odd aBusses) as ae ds fd
  | at 13 = hit even
  | at 14 = hit even
  | at 15 = hit even
  | otherwise = return ()
  where
    at k = floor n `mod` 16 == k
    hit p = do
      ds <- randomRIOs (5e-3,5000e-3)
      as <- randomRIOs (1e-3,3e-2)
      lins (filter p aBusses) as (repeat 0) ds fd

-- | Worker for frequency.
w2 :: (Transport t) => Double -> t -> IO ()
w2 n fd
  | at 0 = do
    f <- randomRIO (80,120)
    ds <- randomRIOs (50e-3,1e2)
    let fs = zipWith (*) [1..] (repeat f)
    lins fBusses fs (zipWith (*) fs (cycle [0.99,1.01])) ds fd
  | at 7 = do
    let fs = repeat 0
    fe <- randomRIOs (100,800)
    lins (filter odd fBusses) fs fe (repeat 0.3) fd
  | at 15 = do
    f <- randomRIO (80,120)
    let fs = zipWith (*) [1..] (repeat f)
    lins fBusses fs (zipWith (*) fs (cycle [0.99,1.01])) (repeat 2) fd
  | at 19 = do
    fs <- randomRIOs (100,8000)
    ds <- randomRIOs (500e-3,8000e-3)
    let fe = repeat 0
    lins fBusses fs fe ds fd
  | otherwise = return ()
  where
    at k = floor n `mod` 23 == k

-- | Worker for pan.
w3 :: (Transport t) => Double -> t -> IO ()
w3 n fd
  | at 0 = do
    pe <- randomRIOs (-1,1)
    lins pBusses (repeat 0) pe (repeat 8) fd
  | otherwise = return ()
  where
    at k = floor n `mod` 7 == k

toA ugs fd = toB aBusses ugs fd
toF ugs fd = toB fBusses ugs fd
toP ugs fd = toB pBusses ugs fd

toB :: (Transport t) => [Int] -> [UGen] -> t -> IO ()
toB bs ugs fd = do
  async fd $ d_recv $ synthdef "anon" (mrg outs)
  send fd $ s_new "anon" (-1) AddToTail 10 []
  where
    outs = zipWith out (map fromIntegral bs) (cycle ugs)
