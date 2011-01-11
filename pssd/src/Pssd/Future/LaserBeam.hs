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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_laserbeam.html>
--
-- Try:
--
-- > > withSC3 noMrBond
--
-- and move mouse in x axis.
--
-- Hm.... it sounds totally different from pd patch.
--
module Pssd.Future.LaserBeam where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

laserBeam :: UGen
laserBeam = out2 sig
  where
    sig = sinOsc ar sig1 0 * amp * gt
    sig1 = sum [lzr f1 l1, lzr f2 l2, lzr f3 l3] * lag cf 10e-3
    lzr freq level = lfSaw ar (freq * cf + level) 0
    cf = ctrl "cf" 174.803
    f1 = ctrl "f1" 33.86
    l1 = ctrl "l1" 56.35
    f2 = ctrl "f2" 75.6
    l2 = ctrl "l2" 213.4
    f3 = ctrl "f1" 52.75
    l3 = ctrl "l3" 15.81
    amp = ctrl "amp" 0.08
    gt = ctrl "gate" 1

laserShooter :: UGen
laserShooter = out bus val
  where
    bus = ctrl "out" 100
    val = mouseX kr 0 1 Linear 0.1 <=* 0.5

laserFilter :: UGen
laserFilter = replaceOut outBus filtered
  where
    filtered = hpf (lpf (in' 2 ar inBus) f) 300
    inBus = ctrl "in" 0
    outBus = ctrl "out" 0
    f = ctrl "f" 15000

mx :: UGen
mx = out outBus (mouseX kr lo hi warp 0.1)
  where
    outBus = ctrl "out" 100
    lo = ctrl "lo" 0
    hi = ctrl "hi" 1
    warp = Warp $ ctrl "warp" 0

my :: UGen
my = out outBus (mouseY kr lo hi warp 0.1)
  where
    outBus = ctrl "out" 100
    lo = ctrl "lo" 0
    hi = ctrl "hi" 0
    warp = Warp $ ctrl "warp" 0

noMrBond :: (Transport t) => t -> IO ()
noMrBond fd = do
  let dr (n,u) = async fd $ d_recv $ synthdef n u
  mapM_ dr [("laserBeam",laserBeam)
           ,("laserShooter",laserShooter)
           ,("laserFilter",laserFilter)
           ,("mx",mx)
           ,("my",my)]
  mkTree laserGraph fd

laserGraph :: SCTree
laserGraph =
  Group 0
    [Group 1
      [Group 15
        [Group 150
          [Synth 1500 "laserShooter"
             ["out":=100]
          ,Synth 1501 "my"
             ["out":=101,"lo":=1e-9,"hi":=15000,"warp":=0]]
        ,Group 151
          [Synth 1510 "laserBeam"
             ["gate":<-100,"cf":=17.4803,"amp":=0.4
             ,"f1":=33.86,"f2":=75.6,"f3":=52.75
             ,"l1":=56.35,"l2":=213.4,"l3":=15.81]
          ,Synth 1511 "laserBeam"
             ["gate":<-100,"cf":=17.861,"amp":=0.3
             ,"f1":=37.86,"f2":=51.46,"f3":=73.23
             ,"l1":=13.47,"l2":=976.4,"l3":=87.52]
        ,Group 152
          [Synth 1520 "laserFilter"
             ["in":=0,"out":=0,"f":<-101]]]]]]
