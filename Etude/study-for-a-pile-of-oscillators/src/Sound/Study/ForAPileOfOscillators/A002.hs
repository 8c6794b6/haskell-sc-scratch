------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with a pile of oscillators, take 2
--
module Sound.Study.ForAPileOfOscillators.A002 where

import Control.Monad (forM_)
import System.Random (mkStdGen, randomRs)
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

import Sound.Study.ForAPileOfOscillators.Common

-- | Write synthdef, reload them, make nodes.
setup :: (Transport t) => t -> IO ()
setup fd = do
  loadSynthdef "piece2" piece2 fd
  addNode 0 a002Tree fd

-- | Write OSC message to file.
writeA002Score :: FilePath -> IO ()
writeA002Score path = do
  let os = zipWith (\t m -> Bundle (NTPr t) [m]) (repeat 0) (treeToOSC a002Tree)
      end = Bundle (NTPr $ 321 * 60 / bpm) []
  writeNRT path $ os ++ [end]

-- | Synth nodes for this piece.
a002Tree :: SCNode
a002Tree =
  Group 0
    [Group 1
       [Group 10 [Synth 1000 "piece2" []]
       ,Group 11 afp2
       ,Group 12 oscs]]

afp2 :: [SCNode]
afp2 =
  [Synth 1001 "ac1"
     ["vc":=18e-3,"vd":=0.125,
      "mix":<-amix,"nfreq":<-anfreq,
      "edgey":<-aedgey,"edur":<-aedur,"crv":=(-12),
      "del":<-adel,"chaos":<-achaos,
      "tfreq":<-atfreq,"tmul":<-atmul,"tos":<-atos,
      "t_trig":<-atrig]
  ,Synth 1002 "fc1"
     ["vc":<-fvc,"vd":<-fvd,
      "fc":<-ffc,"fd":<-ffd,
      "mix":<-fmix,
      "ptc":<-fptc,"vib":<-fvib,"t_trig":<-ftrig]
  ,Synth 1003 "pc1"
     ["fc":<-pfc,"fd":<-pfd,
      "vc":<-pvc,"vd":<-pvd,
      "t_trig":<-ptrig]]

--
-- busses
--

(amix:anfreq:aedgey:aedur:adel:achaos:atfreq:atmul:atos:atrig:_) = [100..]
(fvc:fvd:ffc:ffd:fmix:fptc:fvib:ftrig:fnoise:_) = [200..]
(pvc:pvd:pfc:pfd:ptrig:_) = [300..]

-- | UGen for piece, take 2.
piece2 :: UGen
piece2 = mrg outs
  where
    outs = [outI amix $ mkEnv EnvLin 1 amixE
           ,outI anfreq (0.68 * line kr 0 1 30e-3 DoNothing)
           ,outI aedgey $ clip (sinOsc kr (1/(9*60/bpm)) 0 + 1) 1 0.5
           ,outI aedur $ mkEnv EnvLin 1 edurE
           ,outI adel $ mkEnv EnvLin 1 delE
           ,outI achaos $ mkEnv EnvLin 1 achaosE
           ,outI atfreq $ mkEnv EnvLin 1 tfreqE
           ,outI atmul $ mkEnv EnvLin 1 tmulE
           ,outI atos $ mkEnv EnvLin 1 tosE
           ,outI atrig (impulse kr 1 0)

           ,outI fvc $ mkEnv (EnvNum (-2)) 14000 fvcE
           ,outI fvd $ mkEnv EnvCub 1 fvdE
           ,outI ffc $ mkEnv EnvLin 1 ffcE
           ,outI ffd $ mkEnv EnvLin 1 ffdE
           ,outI fmix (1 * line kr 0 1 4 DoNothing)
           ,outI fptc 7
           ,outI fvib 0.1
           ,outI fnoise (1 * line kr 0 1 4 DoNothing)
           ,outI ftrig (impulse kr 1 0)

           ,outI pvc $ constEnv 0.0
           ,outI pvd $ constEnv 0.99
           ,outI pfc $ constEnv 0.11
           ,outI pfd $ constEnv 0.9
           ,outI ptrig (impulse kr 1 0)]

    outI :: Int -> UGen -> UGen
    outI k ug = out (fromIntegral k) ug

-- | Beats per metre.
bpm :: (Num a) => a
bpm = 80

-- | Helper for making envelope.
mkEnv :: EnvCurve      -- ^ Curve shape
      -> UGen          -- ^ Level scale
      -> [(UGen,UGen)] -- ^ Break points of (time,value)
      -> UGen
mkEnv curve ls bps =
  envGen kr (ctrl "t_trig" 1) ls 0 ts DoNothing $ envCoord bps 1 1 curve
  where
    ts = 60/bpm

------------------------------------------------------------------------------
--
-- For convinience, make every envelope values from between 0 to 1.
--
------------------------------------------------------------------------------
--
-- 32: fvd stop staying as 0, gradually moving toward 1.
-- 64: fvd and fvc stop moving around, fvd reaches to 0.999, fvc to 0.99.
-- 96: chaos reaches to 0, tmul, tfreq, tos triggering quickly.
-- 128: fvd and fvc start moving again.
-- 160: fvd and fvc stop moving around.
-- 192: tmul, tfreq, tos calm down. chaos is 0.2, del is 1.
-- 224: tmul rise to 6.7
-- 252: edur drop to 5e-3

type BreakPoints = [(UGen,UGen)]

-- | Write txt file used as input of tplot.
--
-- For more about tplot, see: <http://www.haskell.org/haskellwiki/Timeplot>
--
-- Below is an example command to create image file of envelopes.
-- Assuming that breakpoint data has been written to \"a002.txt\":
--
-- > $ tplot -if a002.txt -o a002.png -or 1920x1080 -tf 'num' \
-- > > -k amix lines -k achaos lines -k del lines -k edur lines \
-- > > -k tmul lines -k tfreq lines -k tos lines \
-- > > -k fvc lines -k fvd lines -k ffc lines -k ffd lines
--
writeBreakPoints :: FilePath -> IO ()
writeBreakPoints file = writeFile file $ unlines $ concatMap f
  [("amix", amixE)
  ,("achaos", achaosE)
  ,("del", delE)
  ,("edur", edurE)
  ,("tmul", tmulE)
  ,("tfreq", tfreqE)
  ,("tos", tosE)
  ,("fvc", fvcE)
  ,("fvd", fvdE)
  ,("ffc", ffcE)
  ,("ffd", ffdE)]
  where
    f (n,bps) = map (g n) bps
    g n (t,v) = unwords [show $ constantValue t, '=':n, show $ constantValue v]

amixE :: BreakPoints
amixE =
  [(0,0),(17,1),(20,0),(49,0.8),(56,1)
  ,(64,0)
  ,(192,0)
  ,(270,0),(278,1),(279,0),(280,1),(281,0) ]

achaosE :: BreakPoints
achaosE =
  [(0,0)
  ,(64,1),(86,0.87)
  ,(96,0.1),(118,0)
  ,(132,0.01),(140,0),(180,0),(191,0.1)
  ,(193,0.2)
  ,(250,1)]

delE :: BreakPoints
delE =
  [(0,0),(127,0)
  ,(128,0.1),(150,0.3)
  ,(160,1),(180,0)
  ,(192,1)]

edgeyE :: BreakPoints
edgeyE = [(0,0),(1,1)]

edurE :: BreakPoints
edurE =
  [(0,0),(1,450e-3)
  ,(16,5e-3),(32,300e-3),(48,20e-3)
  ,(64,600e-3)
  ,(92,380e-3)
  ,(128,800e-3)
  ,(168,380e-3)
  ,(188,600e-3),(216,900e-3),(232,1200e-3)
  ,(252,900e-3),(298,400e-3),(300,150e-3),(304,5e-3) ]

tmulE :: BreakPoints
tmulE =
  [(0,0),(4,4.5), (92,4.5)
  ,(96,5.8),(180,1.5)
  ,(192,1.5),(205,3),(208,4.3)
  ,(236,5.1)
  ,(252,8.1)
  ,(302,0.8)
  ,(320,0)]

tfreqE :: BreakPoints
tfreqE =
  [(0,0),(3,0.8),(90,0.8)
  ,(98,2.1),(180,3)
  ,(192,1.76)
  ,(250,0.97)]

tosE :: BreakPoints
tosE =
  [(0,0),(3,0.97),(94,0.97)
  ,(102,1.97),(188,1.5)
  ,(192,0.95),(220,0.87)
  ,(222,6.89)
  ,(248,7.89),(300,4),(302,0.2),(309,0)]

fvcE :: BreakPoints
fvcE =
  [(0,0),(8,0.3), (12,0.1),(14,0.5),(18,0.38)
  ,(79, 0.89)
  ,(96, 0.99)
  ,(108,0.01), (117,0.999)
  ,(168,0.9999)
  ,(310,0.9999),(312,0.41)]

fvdE :: BreakPoints
fvdE =
  [(0,0),(32,0),(48,0.5)
  ,(69,0.9999)
  ,(96,0.9999)
  ,(128,0.9999), (136,0.75), (142,0.998), (150,0.85), (152,0.9999)
  ,(162,0.9999)
  ,(296,0.9999)]

ffcE :: BreakPoints
ffcE =
  [(0,1)
  ,(196,1)
  ,(208,5)]

ffdE :: BreakPoints
ffdE =
  [(0,0.5)
  ,(198,0.5)
  ,(202,0.99)]

constEnv :: UGen -> UGen
constEnv val = mkEnv EnvLin 1 [(0,val),(1,val)]