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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_birds.html>
--
module Pssd.Monster.Birds where

import qualified Data.Map as M

import Data.Generics.Uniplate.Data
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

import Pssd.Util

-- | Bird sound, take 1.
bird1 :: UGen
bird1 = out2 sig
  where
    sig = car * mod * amp
    car = sinOsc ar (iFreq + fMod) 0
    fMod = sinOsc ar fFreq 0 * fAmp
    mod = (1 - sinOsc kr aFreq 0 * aAmp)
    amp = ctrl "amp" 0.3
    iFreq = ctrl "if" 220
    fFreq = ctrl "ff" 1
    fAmp = ctrl "fa" 0.8
    aAmp = ctrl "aa" 0.5
    aFreq = ctrl "af" 10

-- | Bird sound, take 2.
--
-- Has evelopes.
--
bird2 :: UGen
bird2 = out2 sig
  where
    sig = sig1 * ampEnv * amp
    ampEnv = e (envPerc atk dcy)
    sig1 = sinOsc ar (iFreq + fOsc) 0 * aOsc
    fOsc = (sinOsc kr fFreq 0 * fAmp) + 1
    aOsc = 1 - (sinOsc kr aFreq 0 * aAmp)
    e shp = envGen kr t_trig 1 0 1 DoNothing shp
    atk = ctrl "atk" 1 * 900e-3
    dcy = ctrl "dcy" 1 * 900e-3
    amp = ctrl "amp" 0.3
    iFreq = ctrl "if" 1 * 7000 + 300
    fFreq = ctrl "ff" 1 * e (envPerc ffAtk ffDcy) * 3000
    ffAtk = ctrl "ffatk" 1 * 900e-3
    ffDcy = ctrl "ffdcy" 1 * 900e-3
    fAmp = ctrl "fa" 0.5 * e (envPerc faAtk faDcy)
    faAtk = ctrl "faatk" 1 * 900e-3
    faDcy = ctrl "fadcy" 1 * 9003-3
    aFreq = ctrl "af" 1 * e (envPerc afAtk afDcy) * 3000
    afAtk = ctrl "afatk" 1 * 900e-3
    afDcy = ctrl "afdcy" 1 * 900e-3
    aAmp = ctrl "aa" 1 * e (envPerc aaAtk aaDcy)
    aaAtk = ctrl "aaatk" 1 * 900e-3
    aaDcy = ctrl "aadcy" 1 * 900e-3
    t_trig = ctrl "t_trig" 1

-- | Send new bird2 synth.
playBird2 :: (Transport t) => t -> IO ()
playBird2 fd = do
  let dusty = out (ctrl "out" 0) (dust 'd' kr (ctrl "freq" 1))
  mapM_ (\(n,u) -> sendSynthdef n u fd) [("bird2",bird2),("dusty",dusty)]
  mkTree b2Tree fd

-- | Node id for bird2 synth.
b2nid :: Int
b2nid = 1000

-- | Synth node for single bird2 synth.
b2Tree :: SCTree
b2Tree =
  Group 0
    [Group 1
      [Synth 999 "dusty" ["out":=100,"freq":=1]
      ,Synth b2nid "bird2"
        ["amp":=0.3,"atk":=1,"dcy":=1,"if":=1
        ,"ff":=1,"ffatk":=1,"ffdcy":=1
        ,"fa":=1,"faatk":=1,"fadcy":=1
        ,"af":=1,"afatk":=1,"afdcy":=1
        ,"aa":=1,"aaatk":=1,"aadcy":=1
        ,"t_trig":<-100]]]

-- | Hints used by gui of bird2 synth.
b2Hints :: Hints
b2Hints = M.fromList $ [("bird2", map (\x -> ParamRange x 0 1) ns)]
  where
    ns = [n | n := _ <- universeBi b2Tree]

-- | Helper for sending n_set to bird2 synth
setBird2 :: (Transport t) => [(String,Double)] -> t -> IO ()
setBird2 ps fd = send fd $ n_set b2nid ps

tripleTailerTreaTroubler :: (Transport t) => t -> IO ()
tripleTailerTreaTroubler = setBird2
    [("if",0.387755),("atk",0.02044082),("dcy",0.204082)
    ,("ff",0.367),("ffatk",0.5714),("ffdcy",0.73469)
    ,("fa",0.918),("faatk",1),("fadcy",0.775)
    ,("af",0.5714),("afatk",0.3873),("afdcy",0.22449)
    ,("aa",0.02408),("aaatk",0.183),("aadcy",0.448)
    ,("t_trig",1)]

speckledThreatedSpew :: (Transport t) => t -> IO ()
speckledThreatedSpew = setBird2
    [("if",0.1836),("atk",0.5918),("dcy",0.3877)
    ,("ff",0.010),("ffatk",0.5366),("ffdcy",0.3469)
    ,("fa",0.2448),("faatk",0.551),("fadcy",0.1224)
    ,("af",0.38775),("afatk",0.99),("afdcy",0.6122)
    ,("aa",0.3469),("aaatk",0.8163),("aadcy",0.653)
    ,("t_trig",1)]

lesserSpottedGrinchwarbler :: (Transport t) => t -> IO ()
lesserSpottedGrinchwarbler = setBird2
    [("if",0.5510),("atk",0.5918),("dcy",0.3877)
    ,("ff",0.070),("ffatk",0.024),("ffdcy",0.5510)
    ,("fa",0.1224),("faatk",0.6326),("fadcy",0.99)
    ,("af",0.0612),("afatk",0.346),("afdcy",0.8163)
    ,("aa",0.6530),("aaatk",0.8163),("aadcy",0.653)
    ,("t_trig",1)]

-- | Birds, take 3.
birds3 :: UGen
birds3 = undefined
