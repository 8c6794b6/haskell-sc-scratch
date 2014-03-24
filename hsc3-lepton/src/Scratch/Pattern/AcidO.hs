{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : non-portable (NoMonomorphismRestriction)

Port of acid otophilia example found in SuperCollider help file.

Try:

>>> import Scratch.Pattern.Acid0
>>> withSC3 reset
>>> setup'acido
>>> setB >> setD dseq0
>>> setD dseq1
>>> setB1 >> setD dseq2
>>> setD dseq3
>>> setB2 >> setD dseq4

-}
module Scratch.Pattern.AcidO where

import Control.Arrow (second)
import System.Random

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton hiding (limiter)

import qualified Data.Map as M

------------------------------------------------------------------------------
-- Setting up

setup'acido :: IO ()
setup'acido = withSC3 $ do
  reset
  let go (n,u) = send . d_recv . synthdef n $ u -- =<< u
  mapM_ go
    [("aokick",kick),("aosnare", snare),("aoclap", clap)
    ,("aohat",hat),("aoacid", acid),("aofx", fx)
    ,("aoacid2", acid2)]
  patchNode (nodify acidNd)


------------------------------------------------------------------------------
-- Nodes

acidNd :: Nd
acidNd =
  grp 0
    [ grp 1
      [ syn' aoid "aoacid2" []]
    , grp 2
      [ syn "aofx" [] ]]

aoid = 1000

------------------------------------------------------------------------------
-- Synthdefs

kick :: UGen
kick =
  let sig0 = clip2 sig1 1 * ("amp"@@0.1)
      sig1 = sig2 * 1.2
      sig2 = sig3 + (sinOsc AR env1m 0.5 * env0)
      sig3 = lpf sig4 (env1m*1.5) * env0
      sig4 = sig5 + noise
      sig5 = lfPulse AR env1m 0 0.5 - 0.5
      env0 = envGen KR 1 1 0 1 DoNothing shp0
      env1 = envGen KR 1 1 0 1 RemoveSynth shp1
      env1m = midiCPS env1
      shp1 = env [110,59,29] [0.005,0.29]
             (map EnvNum [-4,-5]) (-1) (-1)
      shp0 = env [0.5,1,0.5,0] [0.005,0.06,0.26]
             (map EnvNum [-4,-2,-4]) (-1) (-1)
      noise = whiteNoise 'γ' AR
  in  out ("out"@@0) $ dup sig0

snare :: UGen
snare =
  let sig0 = clip2 sig1 1 * ("amp"@@0.8)
      sig1 = osc0 + nz0
      nz0 = nz1 * env2
      nz1 = (bpf nz2 6900 0.6 * 3) + nz2
      nz2 = hpf nz 200 * 2
      nz = whiteNoise 'α' AR * 0.2
      osc0 = osc1 + (sinOsc AR env1m 0.8 * env0)
      osc1 = lpf osc2 (env1m*1.2) * env0
      osc2 = (lfPulse AR env1m 0 0.5 * 1 - 0.5) +
             (lfPulse AR (env1m*1.6) 0 0.5 * 0.5 - 0.25)
      env0 = envGen KR 1 1 0 1 DoNothing shp0
      env1 = envGen KR 1 1 0 1 DoNothing shp1
      env1m = midiCPS env1
      env2 = envGen KR 1 1 0 1 RemoveSynth shp2
      shp0 = env [0.5, 1, 0.5, 0] [0.005, 0.03, 0.10]
             (map EnvNum [-4,-2,-4]) 1 1
      shp1 = env [110, 60, 49] [0.005, 0.1]
             (map EnvNum [-4,-5]) 1 1
      shp2 = env [1, 0.4, 0] [0.05, 0.13]
             (map EnvNum [-2,-2]) 1 1
  in  out ("out"@@0) $ dup sig0

clap :: UGen
clap =
  let sig0 = softClip sig1 * ("amp"@@0.5)
      sig1 = sig2 * 2
      sig2 = nz11 + nz21
      nz11 = bpf nz12 2000 3
      nz12 = hpf nz13 600
      nz13 = nz1 * env1
      nz1 = whiteNoise 'β' AR
      nz21 = bpf nz22 1200 0.7 * 0.7
      nz22 = hpf nz23 1000
      nz23 = nz2 * env2
      nz2 = whiteNoise 'κ' AR
      env1 = envGen KR 1 1 0 1 DoNothing shp1
      env2 = envGen KR 1 1 0 1 RemoveSynth shp2
      shp1 = env [0, 1, 0, 1, 0, 1, 0, 1, 0]
             [0.001, 0.013, 0, 0.01, 0, 0.01, 0, 0.03]
             (map EnvNum [0, -3, 0, -3, 0, -3, 0, -4]) 1 1
      shp2 = env [0, 1, 0] [0.02, 0.3]
             (map EnvNum [0, -4]) 1 1
  in  out ("out"@@0) $ dup sig0

hat :: UGen
hat =
  let n = 5
      n2 = 8
      oscs1 = mixFill n $ \k ->
        let k' = constant k
            n' = constant n
            g = mkStdGen 0xf8988abde
            g' = mkStdGen 0x8ffaba8989a132
            f1 = fst $ randomR (0,4) g
            f2 = fst $ randomR (0,4) g'
        in  sinOsc AR
            (midiCPS $ linLin k' 0 (n'-1) 42 74 + f1)
            (sinOsc AR
             (midiCPS $ linLin k' 0 (n'-1) 78 80 + f2) 0 * 12)
            * (1/n')
      noise4 = whiteNoise 'θ' AR
      noise3 = mixFill n2 $ \k ->
        let fadd = fst $ randomR (0,4) (mkStdGen 0xabad8234bc3)
            k' = constant k
            n2' = constant n2
            frq = linLin k' 0 (n2'-1) 40 50 + fadd
        in  combN noise4 0.04 frq 0.1 * (1/n2') + noise4
      sig0 = softClip sig1 * ("amp"@@0.3) * 0.2
      sig1 = noise0 + oscs0
      oscs0 = bHiPass oscs1 1000 2 * env1
      noise0 = bHiPass noise1 1000 1.5 * env2
      noise1 = bLowShelf noise2 3000 0.5 (-6)
      noise2 = bpf noise3 6000 0.9 * 0.5 + noise3
      env1 = envGen KR 1 1 0 1 DoNothing shp1
      env2 = envGen KR 1 1 0 1 RemoveSynth shp2
      shp1 = env [0, 1.0, 0] [0.001, 0.2]
             (map EnvNum [0, -12]) 1 1
      shp2 = env [0, 1.0, 0.05, 0] [0.002, 0.05, 0.03]
             (map EnvNum [0, -4, -4]) 1 1
  in  out ("out"@@0) $ dup sig0

{-
hat :: IO UGen
hat = do
  let n = 5
      n2 = 8

  oscs1 <- mixFillM n $ \k -> do
              let k' = constant k
                  n' = constant n
              f1 <- getStdRandom (randomR (0,4))
              f2 <- getStdRandom (randomR (0,4))
              return $ sinOsc AR
                     (midiCPS $ linLin k' 0 (n'-1) 42 74 + f1)
                     (sinOsc AR
                      (midiCPS $ linLin k' 0 (n'-1) 78 80 + f2) 0 * 12)
                     * (1/n')

  let noise4 = whiteNoise 'θ' AR
  noise3 <- mixFillM n2 $ \k -> do
              fAdd <- getStdRandom (randomR (0,4))
              let k' = constant k
                  n2' = constant n2
                  frq = linLin k' 0 (n2'-1) 40 50 + fAdd
              return $ combN noise4 0.04 frq 0.1 * (1/n2') + noise4

  let sig0 = softClip sig1 * ("amp"@@0.3)
      sig1 = noise0 + oscs0
      oscs0 = bHiPass oscs1 1000 2 * env1
      noise0 = bHiPass noise1 1000 1.5 * env2
      noise1 = bLowShelf noise2 3000 0.5 (-6)
      noise2 = bpf noise3 6000 0.9 * 0.5 + noise3
      env1 = envGen KR 1 1 0 1 DoNothing shp1
      env2 = envGen KR 1 1 0 1 RemoveSynth shp2
      shp1 = env [0, 1.0, 0] [0.001, 0.2]
             (map EnvNum [0, -12]) 1 1
      shp2 = env [0, 1.0, 0.05, 0] [0.002, 0.05, 0.03]
             (map EnvNum [0, -4, -4]) 1 1
  return $ out ("out"@@0) $ dup sig0
-}

acid :: UGen
acid =
  let sig0 = sig1 * env1
      sig1 = rlpf sig2 (midiCPS $ ptc0 + env2) 0.3
      sig2 = lfPulse AR (midiCPS ptc0) 0 0.51 * 2 - 1
      env1 = envGen KR gt 1 0 1 DoNothing shp1 * ("amp"@@0.1)
      env2 = envGen KR gt 1 0 1 DoNothing shp2
      shp1 = env [0, 1.0, 0, 0] [0.001, 2.0, 0.04]
             (map EnvNum [0, -4, -4]) 1 1
      shp2 = env [0, 70.0, 0.8, 0.8] [0.001, 0.8, 0]
             (map EnvNum [-4, -4, -4]) (-1) 1
      ptc0 = lag ("pitch"@@50) (0.12 * (1 - trig gt 0.001) * gt)
      gt = "gate"@@1
  in  out ("out"@@0) $ dup sig0

acid2 :: UGen
acid2 =
  let sig0 = sig1 * env1
      sig1 = rlpf sig2 (midiCPS $ ptc0 + env2) 0.3
      sig2 = lfPulse AR (midiCPS ptc0) 0 0.51 * 2 - 1
      env1 = envGen KR gt 1 0 1 DoNothing shp1 * ("amp"@@0.1)
      env2 = envGen KR gt 1 0 1 DoNothing shp2
      shp1 = env [0, 1.0, 0, 0] [0.001, 2.0, 0.04]
             (map EnvNum [0, -4, -4]) 1 1
      shp2 = env [0, 70.0, 0.8, 0.8] [0.001, 0.8, 0]
             (map EnvNum [-4, -4, -4]) (-1) 1
      ptc0 = lag ("pitch"@@50) (0.12 * (1 - trig gt 0.001) * gt)
      gt = toggleFF (gt'+ tDelay gt' (0.25 * "delta"@@0.1))
      gt' = "t_gate"@@1
  in  out ("out"@@0) $ dup sig0

fx :: UGen
fx =
  let sig0 = limiter sig1 1.0 0.02
      sig1 = hpf (sig2 *1.2) 40
      sig2 = freeVerb2 (bpf (mceChannel 0 sig3) 3500 1.5)
                       (bpf (mceChannel 1 sig3) 3500 1.5)
                       1.0 0.95 0.15 *
             envGen KR gt 1 0 1 DoNothing shp1 +
             sig3
      sig3 = in' 2 AR ("out"@@0)
      shp1 = env [0.02, 0.3, 0.02] [0.4, 0.01]
             (map EnvNum [3, -4]) 1 1
      gt = 1 - trig ("gate"@@1) 0.01
  in  replaceOut ("outBus"@@0) sig0

------------------------------------------------------------------------------
-- Helper

durs = replicate 16 (60/(bpm*4))

bpm = 130
bar = (60/130) * 4

setB =
  leptseq =<< bundle' bar 0 [l_new "bseq" (mkB bseq)]

setB1 = do
  leptseq =<< bundle' bar 0 [l_new "bseq" (mkB1 bseq1)]

setB2 = do
  gen <- newStdGen
  leptseq =<< bundle' bar 0 [l_new "bseq" (mkB1 (bseq2 gen))]

setD dsq = withLept $ do
  sendOSC =<< liftIO (bundle' bar 0 [l_new "dseq" (pforever (mkD dsq))])

delD = leptseq =<< bundle' bar 0 [l_free "dseq"]
delB = leptseq =<< bundle' bar 0 [l_free "bseq"]

mkB sco =
  pforever . pnset aoid .
  map (second (pforever . pconcat . map pdouble)) $
  ("dur", durs) : sco

mkB1 sco =
  pforever . pnset aoid .
  map (second pforever) $
  ("dur", (pconcat $ map pdouble durs)) : sco

mkD scos = ppar (map f scos) where
  ds = map pdouble
  f (n,as) =
    pforever $ psnew ("ao"++n) Nothing AddToTail 1
    [("dur", pconcat $ ds durs)
    ,("amp", pconcat $ ds $ map (*0.25) as)]

------------------------------------------------------------------------------
-- Patterns

type Sco = [(String,[Double])]

bseq :: Sco
bseq =
  [ ("t_gate", [1,1,1,1, 1,1,1,1, 0,1,0,1, 1,1,1,0])
  , ("delta",  [1,1,0,2, 1,1,0,0, 2,0,2,0, 1,2,0,4])
  , ("pitch",
     map (+38) [-24,-12,0,-12, 0,-12,10,12, 0,7,-7,0, -11,1,13,15])]

bseq1 =
  let i = pint; d = pdouble; ds = map pdouble;
      pri x = preplicate (pint x); prnd = prand (i 1) . ds
  in  [ ("t_gate", pri 16 (prnd [1,1,0,1]))
      , ("delta",  pri 16 (prnd [0,1,2,4]))
      , ("pitch",
         pforever (d 38) +@
         pconcat [ {- pconcat (ds [-24,-12,0,-12])
                 , pconcat (ds [0,-12,10,12])
                 -- , pconcat (ds [0,7,-7,0])
                 , -} pri 12 (prnd [-24,-12,-7,0,7,12])
                 , pri 4 (prnd [-11,1,13,15])
                 -- , pconcat (ds [-11,1,13,15])
                 ])
      ]

bseq2 g0 =
  let d = pdouble; ds = map pdouble;
      g1 = snd $ next g0; g2 = snd $ next g1; g3 = snd $ next g2
      gates = [0,1]; deltas = [0,1,2,4]
      ptch1s = [-24,-12,-7,0,7,12]
      ptch2s = [-11,1,13,15]
      values n vs g = map (vs!!) $ take n $ randomRs (0,length vs-1) g
  in  [ ("t_gate", pconcat $ ds (values 16 gates g0))
      , ("delta",  pconcat $ ds (values 16 deltas g1))
      , ("pitch",
         pforever (d 38) +@
         pconcat [ pconcat $ ds (values 12 ptch1s g2)
                 , pconcat $ ds (values 4 ptch2s g3) ])
      ]

dseq0 :: Sco
dseq0 =
  [ ("kick",  [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0])
  , ("snare", [0,0,0,0, 4,0,0,2, 0,0,0,0, 4,0,0,0])
  , ("clap",  [0,0,0,0, 0,0,0,0, 0,0,0,0, 4,0,0,0])
  , ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])]

dseq1 :: Sco
dseq1 =
  [ ("kick",  [1,0,0,0, 0,0,0,0, 1,0,0,1, 0,0,1,0])
  , ("snare", [0,0,0,0, 0,0,0,2, 0,2,1,0, 4,3,3,3])
  , ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0])
  , ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])]

dseq2 :: Sco
dseq2 =
  [ ("kick",  [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])
  , ("snare", [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])
  , ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0])
  , ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])]

dseq3 :: Sco
dseq3 =
  [ ("kick",  [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0])
  , ("snare", [0,0,0,0, 0,0,0,2, 0,0,0,0, 0,0,0,0])
  , ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0])
  , ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])]

dseq4 :: Sco
dseq4 =
  [ ("kick",  [1,0,0,1, 0,0,1,0, 0,1,0,0, 1,0,1,0])
  , ("snare", [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0])
  , ("clap",  [4,0,3,0, 4,0,0,0, 0,0,0,0, 0,3,0,2])
  , ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])
  ]

dseq5 :: Sco
dseq5 =
  [ ("kick",  [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])
  , ("snare", [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])
  , ("clap",  [4,0,3,0, 4,0,0,0, 0,0,0,0, 0,3,0,2])
  , ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])
  ]

dseq6 :: Sco
dseq6 =
  [ ("kick",  [1,0,0,1, 0,0,1,0, 0,1,0,0, 1,0,1,0])
  , ("snare", [0,2,0,0, 2,0,2,0, 2,0,0,2, 0,0,2,0])
  , ("clap",  [4,0,3,0, 4,0,0,0, 0,0,0,0, 0,3,0,2])
  , ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])
  ]

dup a = mce [a,a]

env ls ts cs r n = Envelope ls ts cs (Just r) (Just n)

{-- -------------------------------------------------------------------------
 -- Workspace

setD dseq4

withSC3 $ flip send $ dumpOSC HexPrinter
withSC3 $ flip send $ dumpOSC NoPrinter

leptseq l_dump
leptseq $ l_free "spe-lo"
leptseq l_freeAll
leptseq $ l_free "dseq"
leptseq $ l_free "bseq"

setup'acido
delD
delB
setB >> setD dseq0
setB1
setB1 >> setD dseq6
setB
setB2
setB2 >> setD dseq5
setD dseq0
setD dseq1
setD dseq2
setD dseq3
setD dseq4
setD dseq5
setD dseq6

-}

-- RIP.
