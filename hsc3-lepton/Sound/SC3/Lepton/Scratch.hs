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
-- Scratch!!!
--

module Sound.SC3.Lepton.Scratch where

import Control.Arrow (second)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (zipWithM_)
import System.Random (StdGen, newStdGen, randomRs)
import Data.Map ((!))
import Data.Traversable (sequenceA)
import qualified Data.Map as M


import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID

import Sound.SC3.Lepton.Pattern
import Sound.SC3.Lepton.Tree

------------------------------------------------------------------------------
--
-- Patterns
--
------------------------------------------------------------------------------

main :: IO ()
main = withSC3 go

-- | Load synth def and play the pattern.
go :: (Transport t) => t -> IO ()
go fd = do
  async fd . d_recv . synthdef "speSynth" =<< speSynth
  zipWithM_ f (repeat 1) =<< runPIO pspe2
  where
    f t v = do
      send fd $ s_new "speSynth" (-1) AddToTail 1 [("freq",midiCPS v)]
      threadDelay (floor $ t * 0.13 * 1e6)

go2 n fd = do
  async fd . d_recv . synthdef "speSynth" =<< speSynth
  zipWithM_ f [n..] =<< runPIO pspe2
  where
    f nid pch = do
      send fd $ s_new "speSynth" nid AddToTail 1 [("freq",midiCPS pch)]
      threadDelay (floor $ 0.13 * 1e6)

-- | Synthdef for spe example.
speSynth :: IO UGen
speSynth = do
  dl <- randomRs (0,0.05) `fmap` newStdGen
  dr <- randomRs (0,0.05) `fmap` newStdGen
  return $ out 0 $ mkSig dl dr
  where
    mkSig dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
    v = rlpf (lfSaw ar freq 0 * evl) nz 0.1
    f a b = allpassN b 0.05 a 4
    evl = envGen kr 1 1 0 1 RemoveSynth shp * 0.3
    shp = envPerc 10e-3 1
    nz = midiCPS (lfNoise1 'z' kr 1 * 36 + 110)
    freq = control kr "freq" 440

pspe' =
  pcycle
    [prand 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prand 1 [2..5])
       [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
    ,prand (prand 1 [3..9])
       [74,75,77,79,81]]

pspe2 =
  pcycle
    [pchoose 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange 2 5)
       [60, pchoose 1 [63,65], 67, pchoose 1 [70,72,74]]
    ,pchoose (prange 3 9)
       [74,75,77,79,81]]

-- like glass?
p2 =
  pcycle [a, b, c]
  where
    a = pseq (prand 1 [2,4])
        [pseq (prand 1 [2,4,8]) [plist [60,63]]
        ,pseq (prand 1 [2,4,8]) [plist [58,60,63]]
        ,pseq (prand 1 [2,4,8]) [plist [63,60]]
        ,pseq (prand 1 [2,4,8]) [plist [58,63,60]]
        ,pseq (prand 1 [4,8]) [plist [63]]
        ,pseq (prand 1 [2,4,8]) [plist [58,63]]
        ,pseq (prand 1 [2,4,8]) [plist [60,58]]
        ,pseq (prand 1 [4,8]) [plist [60]]]
    b = fmap (+(-2)) a
    c = fmap (+3) a

pspe =
  pcycle
    [prand (pval 1)
     [pempty, plist [24,31,36,43,48,55]]

    ,pseq (prand (pval 1) [pval 2, pval 3, pval 4, pval 5])
     [pval 60
     ,prand (pval 1) [pval 63, pval 65]
     ,pval 67
     ,prand (pval 1) [pval 70, pval 72, pval 74]]

    ,prand (prand (pval 1)
            [pval 3, pval 4, pval 5, pval 6, pval 7, pval 8, pval 9])
     [pval 74, pval 75, pval 77, pval 79, pval 81]]

------------------------------------------------------------------------------
--
-- Synth Nodes
--
------------------------------------------------------------------------------

-- | Sample message returned from scsynth server, without group other
-- than default.
oscList1 :: OSC
oscList1 = Message "/g_queryTree.reply"
                [Int 1, -- containing control parameters
                 Int 1,Int 2, -- default group, with 2 child elements
                 Int 1000,Int (-1),
                 String "simplePercSine",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",Float 0.0,
                 String "amp",Float 0.10000000149011612,
                 String "freq",Float 440.0,
                 String "out",Float 0.0,
                 Int 1001,Int (-1),
                 String "simplePercSine",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",Float 0.0,
                 String "amp",Float 0.10000000149011612,
                 String "freq",Float 440.0,
                 String "out",Float 0.0]

tree1 :: SCNode
tree1 =
  Group 0
    [Group 1
      [Synth 1000 "simplePercSine"
        ["sustain" := 0.800000011920929,
         "trig" :<- 101,
         "amp" := 0.10000000149011612,
         "freq" := 440,
         "out" := 0],
       Group 10
         [Group 100
           [Group 101
             [Synth 1011 "simplePercSine"
              ["sustain" := 0.8,
               "trig" :<- 102,
               "amp" := 0.1,
               "freq" := 330,
               "out" := 0]]]],
       Synth 1001 "simplePercSine"
         ["sustain" := 0.800000011920929,
          "trig" :<- 103,
          "amp" := 0.10000000149011612,
          "freq" := 440,
          "out" := 0]]]

oscList2 :: OSC
oscList2 =
    Message "/g_queryTree.reply"
                [Int 1, -- containing control parameters
                 Int 0,Int 1, -- root node, with 1 child element
                 Int 1,Int 5, -- default group, with 5 child element
                 Int 2,Int 0, -- group 2, no child element
                 Int 3,Int 4, -- group 3, 4 child elements.
                 Int 1002,Int (-1), -- node 1002,
                 String "param",Int 4, -- name is "param", 4 control params.
                 String "idx",String "c103", -- idx, from control bus 103.
                 String "parambuf",Float 10.0,
                 String "trig",String "c102",
                 String "out",Float 100.0,
                 Int 1003,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c103",
                 String "parambuf",Float 12.0,
                 String "trig",String "c102",
                 String "out",Float 101.0,
                 Int 1004,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c203",
                 String "parambuf",Float 20.0,
                 String "trig",String "c202",
                 String "out",Float 200.0,
                 Int 1005,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c203",
                 String "parambuf",Float 22.0,
                 String "trig",String "c202",
                 String "out",Float 201.0,
                 Int 4,Int 2,
                 Int 1000,Int (-1),
                 String "para4",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",String "c102",
                 String "amp",String "c100",
                 String "freq",String "c101",
                 String "out",Float 104.0,
                 Int 1001,Int (-1),
                 String "para4",Int 5,
                 String "sustain",Float 2.0,
                 String "trig",String "c202",
                 String "amp",String "c200",
                 String "freq",String "c201",
                 String "out",Float 204.0,
                 Int 5,Int 2,
                 Int 1006,Int (-1),
                 String "simpleReverb",Int 4,
                 String "damp",Float 0.5,
                 String "room",Float 0.8999999761581421,
                 String "mix",Float 0.5,
                 String "in",Float 104.0,
                 Int 1007,Int (-1),
                 String "simpleReverb",Int 4,
                 String "damp",Float 0.8999999761581421,
                 String "room",Float 0.5,
                 String "mix",Float 0.5,
                 String "in",Float 204.0,
                 Int 6,Int 2,
                 Int 1008,Int (-1),
                 String "simplePanGain",Int 3,
                 String "pan",Float (-0.800000011920929),
                 String "gain",Float 1.0,
                 String "bus",Float 104.0,
                 Int 1009,Int (-1),
                 String "simplePanGain",Int 3,
                 String "pan",Float 0.800000011920929,
                 String "gain",Float 1.0,
                 String "bus",Float 204.0]

oscList3 :: OSC
oscList3 =
    Message "/g_queryTree.reply"
    [Int 1,
     Int 1, Int 2,
     Int 2, Int 0,
     Int 3, Int 0]


-- example for Tree module

playFooBar :: (Transport t) => t -> IO ()
playFooBar fd = do
  mapM (\(n,u) -> async fd . d_recv $ synthdef n u) [("foo",foo),("bar",bar)]
  addNode 0 nodes fd

-- | Node graph for playing foos and bars.
-- Two fmod control synthes are implicitly added with sending to same bus.
nodes :: SCNode
nodes =
  Group 10
    [Group 100
      [Synth 1000 "foo"
         ["out":=100,"amp":=100,"freq":=1.66]
      ,Synth 1001 "foo"
         ["out":=101,"amp":=80,"freq":=3.33]
      ,Synth 1002 "foo"
         ["out":=101,"amp":=233,"freq":=0.05]]
    ,Group 110
      [Synth 1100 "bar"
         ["amp":=0.05,"pan":=0.5,"freq":=110,"fmod":<-100]
      ,Synth 1101 "bar"
         ["amp":=0.05,"pan":=(-0.5),"freq":=330,"fmod":<-101]]]

nTrace :: (Transport t) => NodeId -> t -> IO OSC
nTrace nid fd = do
  async fd $ notify True
  send fd $ n_query [nid]
  wait fd "/n_info"

replaceNode :: (Transport t) => NodeId -> SCNode -> t -> IO ()
replaceNode nid new fd = do
  let (h:rest) = treeToNew nid new
  send fd $ Bundle immediately (f h:rest)
  where
    f x = case x of
      Message "/s_new" (n:i:a:r) -> Message "/s_new" (n:i:Int 4:r)
      Message "/g_new" (i:a:r)   -> Message "/g_new" (i:Int 4:r)
      _                          -> error $ "cannot replace" ++ show x

foo :: UGen
foo = out outBus (sinOsc kr freq 0 * amp)

bar :: UGen
bar = out 0 $ pan2 (saw ar (fmod + freq) * amp) pan 1

outBus, amp, freq, pan, fmod :: UGen
outBus = control kr "out" 0
amp = control kr "amp" 0.3
freq = control kr "freq" 440
pan = control kr "pan" 0
fmod = control kr "fmod" 0

-- | Play the pattern.
goBuzz :: (Transport t) => t -> IO ()
goBuzz fd = do
  async fd $ d_recv $ synthdef "buzz" buzz
  pms <- runPIO . sequenceA . M.fromList $ pBuzz
  mapM_ f pms
  where
    f m = do
      send fd $ s_new "buzz" (-1) AddToTail 1 (M.assocs m)
      threadDelay $ floor $ (m!"dur") * 1e6 * (60/bpm)
    bpm = 160

-- | UGen for buzz.
buzz :: UGen
buzz = out 0 $ pan2 sig pan 1
  where
    sig = sinOsc ar freq 0 * amp * e
    e = linen tr 5e-3 1 (10e-3+(220/freq)) RemoveSynth ^ 2
    amp = control kr "amp" 0.3
    freq = control kr "freq" 440
    pan = control kr "pan" 0
    tr = tr_control "t_trig" 1

-- Pattern for amp, dur, freq, and pan.
pBuzz =
  [("amp", pcycle [0.3, 0.1,  0.1,   0.3,  0.1,  0.1,  0.1])
  ,("dur", pcycle [1,   0.55, 0.45,  0.54, 0.46, 0.53, 0.47])
  ,("freq", fmap midiCPS $
            pcycle [48, pchoose 13 cm, 53, pchoose 13 fm
                   ,48, pchoose 13 cm, 43, pchoose 13 g7
                   ,48, pchoose 13 cm, 53, pchoose 13 fm
                   ,50, pchoose 6 fm, 43, pchoose 6 g7
                   ,48, pchoose 6 cm, 55, pchoose 6 cm])
  ,("pan", pcycle [plist [-1,-0.9..1], plist [1,0.9..(-1)]])]
  where
    cm = [55, 67,72,75,79,84,87]
    fm = [60, 68,72,77,80,84,89]
    g7 = [50, 67,71,74,77,79,83]

lam1 :: ((R a1 -> [a1]) -> [a]) -> R a
lam1 f = R $ \g -> f (`unR` g)

lam2 f = R $ \g -> unR (f (`unR` g)) g

fooP :: (R a -> R b)
fooP = undefined

foo' :: (StdGen -> [a]) -> StdGen -> [b]
foo' = unR . fooP . R

foo'' f = unR . f . R

-- bar :: (R a -> R b) -> (StdGen -> [a]) -> StdGen -> [b]
-- bar f = unR . f . R

bar' :: (R a -> R b) -> (StdGen -> [a]) -> R b
bar' f = R . unR . f . R

bar'' f = show . f . length

-- buzz g f = let h = (bar foo f g) in h

-- instance Plam V where
-- vlam f = V $ "plam " ++ unV (f (V ""))

class Ps p where
  ps :: (Show a, Show b, Show c) => p (a->b->c) -> p (a->b) -> p a -> p c

class Pk p where
  pk :: (Show b) => p a -> p b -> p a

instance Pk R where
  pk (R a) (R b) = R $ \g -> a g

instance Ps R where
  ps (R px) (R py) (R pz) = R $ \g -> f (px g) (py g) (pz g)
    where
      f :: [a -> b -> c] -> [a -> b] -> [a] -> [c]
      f (x:xs) (y:ys) (z:zs) = x z (y z) : f xs ys zs
      f _ _ _ = []

instance Pk S where
  pk a b = S $ const $ "pk (" ++ showP a ++ ") (" ++ showP b ++ ")"



------------------------------------------------------------------------------
-- Simpler implementation for string representation of patterns.
-- String representation of pattern /might/ take argument for showing variable.
-- This implementation does not take argument.
--
-- Though still not sure /plam/ and /papp/ class would be made or not.
-- When I hit an idea for implementing R of lam and app, may remove this.
--
newtype V s = V {unV :: (Show s) => String}

viewP :: (Show a) => V a -> String
viewP = unV

instance (Show a) => Show (V a) where
  show (V a) = a

instance (Show a, Eq a) => Eq (V a) where
  V a == V b = a == b

instance Typeable1 V where
  typeOf1 _ = mkTyConApp (mkTyCon "Sound.SC3.Lepton.Pattern.V") []

instance (Num a) => Num (V a) where
  V a + V b = V $ a ++ "+" ++ b
  V a * V b = V $ a ++ "*" ++ b
  abs (V v) = V $ "abs (" ++ v ++ ")"
  negate (V n) = V $ "negate (" ++ n ++ ")"
  signum (V n) = V $ "signum (" ++ n ++ ")"
  fromInteger n = V $ "pval " ++ show n

instance (Fractional a) => Fractional (V a) where
  V a / V b = V $ a ++ " / " ++ b
  fromRational n = V $ "pval " ++ show (fromRational n :: Double)

instance (Show a, Enum a) => Enum (V a) where
  pred (V n) = V $ "pred (" ++ n ++ ")"
  succ (V n) = V $ "succ (" ++ n ++ ")"
  fromEnum (V _) = undefined
  toEnum n = V $ "pval " ++ show n

--
-- Instance definitions for expressions
--

instance Pval V where
  pval x = V $ "pval " ++ show x

instance Pempty V where
  pempty = V "pempty"

instance Plist V where
  plist xs = V $ "plist " ++ showList xs ""

instance Pseq V where
  pseq (V n) ps = V $ "pseq (" ++ n ++ ") " ++ showList ps ""

instance Prand V where
  prand (V n) ps = V $ "prand (" ++ n ++ ") " ++ showList ps ""

instance Prandom V where
  prandom = V "prandom"

instance Pshuffle V where
  pshuffle ps = V $ "pshuffle " ++ showList ps ""

instance Prange V where
  prange (V lo) (V hi) = V $ "prange (" ++ lo ++ ") (" ++ hi ++ ")"

instance Pchoose V where
  pchoose (V n) ps = V $ "pchoose (" ++ n ++ ") " ++ showList ps ""

instance Pcycle V where
  pcycle ps = V $ "pcycle " ++ showList ps ""

instance Prepeat V where
  prepeat p = V $ "prepeat " ++ show p

instance Pforever V where
  pforever (V p) = V $ "pforever (" ++ p ++ ")"
