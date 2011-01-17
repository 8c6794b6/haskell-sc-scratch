{-# LANGUAGE PackageImports #-}
------------------------------------------------------------------------------
-- |
-- Module      : SCExamples.Pieces.AcidOtopholia
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Exercise for implementing sc3 example code from
-- /acid_otophilia.scd/.
--
-- @hat@ synthdef differs from sclang version.
--

module SCExamples.Pieces.AcidOtophilia where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import "mtl" Control.Monad.State
import Data.Char (toLower)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Data.Map (Map)
import System.Random

import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Monadic

import Sound.SC3.Lepton
import qualified Sound.SC3.Lepton.UGen.ControlArg as A

runAcidOtophilia :: IO ()
runAcidOtophilia = do
  ps 130 >> return ()

setAcidOtophilia :: Transport t => t -> IO ()
setAcidOtophilia fd = do
  zipWithM (lsd fd)
     ["kick", "snare", "clap", "hat", "acid", "fx"]
     [kick, snare, clap, hat, acid, return fx]
  mkTree acidTree fd
   where
     lsd :: Transport t => t -> String -> (IO UGen) -> IO OSC
     lsd fd name ioUGen =
         ioUGen >>= \ug -> loadSynthdef name ug fd

kick :: IO UGen
kick = do
  noise <- whiteNoise ar
  let sig0 = clip2 sig1 1
      sig1 = sig2 * 1.2
      sig2 = sig3 + (sinOsc ar env1m 0.5 * env0)
      sig3 = lpf sig4 (env1m*1.5) * env0
      sig4 = sig5 + noise
      sig5 = lfPulse ar env1m 0 0.5 * 1 - 0.5

      env0 = envGen ar 1 1 0 1 RemoveSynth shp0
      env1 = envGen ar 1 1 0 1 RemoveSynth shp1
      env1m = midiCPS env1

      shp1 = env [110,59,29] [0.005,0.29]
             (map EnvNum [-4,-5]) (-1) (-1)
      shp0 = env [0.5,1,0.5,0] [0.005,0.06,0.26]
             (map EnvNum [-4,-2,-4]) (-1) (-1)

  return $ out A.outBus $ dup sig0

snare :: IO UGen
snare = do
  nz <- (*0.2) <$> whiteNoise ar
  let sig0 = clip2 sig1 1 * (A.amp {controlDefault=0.8})
      sig1 = osc0 + nz0

      nz0 = nz1 * env2
      nz1 = (bpf nz2 6900 0.6 * 3) + nz2
      nz2 = hpf nz 200 * 2

      osc0 = osc1 + (sinOsc ar env1m 0.8 * env0)
      osc1 = lpf osc2 (env1m*1.2) * env0
      osc2 = (lfPulse ar env1m 0 0.5 * 1 - 0.5) +
             (lfPulse ar (env1m*1.6) 0 0.5 * 0.5 - 0.25)

      env0 = envGen kr 1 1 0 1 DoNothing shp0
      env1 = envGen kr 1 1 0 1 DoNothing shp1
      env1m = midiCPS env1
      env2 = envGen kr 1 1 0 1 RemoveSynth shp2

      shp0 = env [0.5, 1, 0.5, 0] [0.005, 0.03, 0.10]
             (map EnvNum [-4,-2,-4]) 1 1
      shp1 = env [110, 60, 49] [0.005, 0.1]
             (map EnvNum [-4,-5]) 1 1
      shp2 = env [1, 0.4, 0] [0.05, 0.13]
             (map EnvNum [-2,-2]) 1 1

  return $ out A.outBus $ dup sig0

clap :: IO UGen
clap = do
  nz1 <- whiteNoise ar
  nz2 <- whiteNoise ar
  let sig0 = softClip sig1 * (A.amp {controlDefault=0.5})
      sig1 = sig2 * 2
      sig2 = nz11 + nz21

      nz11 = bpf nz12 2000 3
      nz12 = hpf nz13 600
      nz13 = nz1 * env1

      nz21 = bpf nz22 1200 0.7 * 0.7
      nz22 = hpf nz23 1000
      nz23 = nz2 * env2

      env1 = envGen kr 1 1 0 1 DoNothing shp1
      env2 = envGen kr 1 1 0 1 RemoveSynth shp2

      shp1 = env [0, 1, 0, 1, 0, 1, 0, 1, 0]
             [0.001, 0.013, 0, 0.01, 0, 0.01, 0, 0.03]
             (map EnvNum [0, -3, 0, -3, 0, -3, 0, -4]) 1 1
      shp2 = env [0, 1, 0] [0.02, 0.3]
             (map EnvNum [0, -4]) 1 1

  return $ out A.outBus $ dup sig0

hat :: IO UGen
hat = do
  let n = 5
      n2 = 8

  oscs1 <- mixFillM n $ \k -> do
              let k' = constant k
                  n' = constant n
              f1 <- getStdRandom (randomR (0,4))
              f2 <- getStdRandom (randomR (0,4))
              return $ sinOsc ar
                     (midiCPS $ linLin k' 0 (n'-1) 42 74 + f1)
                     (sinOsc ar
                      (midiCPS $ linLin k' 0 (n'-1) 78 80 + f2) 0 * 12)
                     * (1/n')

  noise4 <- whiteNoise ar
  noise3 <- mixFillM n2 $ \k -> do
              fAdd <- getStdRandom (randomR (0,4))
              let k' = constant k
                  n2' = constant n2
                  frq = linLin k' 0 (n2'-1) 40 50 + fAdd
              return $ combN noise4 0.04 frq 0.1 * (1/n2') + noise4

  let sig0 = softClip sig1 * (A.amp {controlDefault=0.3})
      sig1 = noise0 + oscs0

      oscs0 = bHiPass oscs1 1000 2 * env1

      noise0 = bHiPass noise1 1000 1.5 * env2
      noise1 = bLowShelf noise2 3000 0.5 (-6)
      noise2 = bpf noise3 6000 0.9 * 0.5 + noise3

      env1 = envGen kr 1 1 0 1 DoNothing shp1
      env2 = envGen kr 1 1 0 1 RemoveSynth shp2

      shp1 = env [0, 1.0, 0] [0.001, 0.2]
             (map EnvNum [0, -12]) 1 1
      shp2 = env [0, 1.0, 0.05, 0] [0.002, 0.05, 0.03]
             (map EnvNum [0, -4, -4]) 1 1

  return $ out A.outBus $ dup sig0

acid :: IO UGen
acid = do
  let sig0 = sig1 * env1
      sig1 = rlpf sig2 (midiCPS $ ptc0 + env2) 0.3
      sig2 = lfPulse ar (midiCPS ptc0) 0 0.51 * 2 - 1

      env1 = envGen kr A.gate 1 0 1 DoNothing shp1 *
             A.amp {controlDefault=0.1}
      env2 = envGen kr A.gate 1 0 1 DoNothing shp2

      shp1 = env [0, 1.0, 0, 0] [0.001, 2.0, 0.04]
             (map EnvNum [0, -4, -4]) 1 1
      shp2 = env [0, 70.0, 0.8, 0.8] [0.001, 0.8, 0]
             (map EnvNum [-4, -4, -4]) (-1) 1

      ptc0 = lag (A.pitch {controlDefault=50})
             (0.12 * (1 - trig A.gate 0.001) * A.gate)

  return $ out A.outBus $ dup sig0

fx ::UGen
fx = out A.outBus sig0
    where
      sig0 = limiter sig1 1.0 0.02
      sig1 = hpf (sig2 *1.2) 40
      sig2 = freeVerb2 (bpf (mceChannel 0 sig3) 3500 1.5)
                       (bpf (mceChannel 1 sig3) 3500 1.5)
                       1.0 0.95 0.15 *
             envGen kr gt 1 0 1 DoNothing shp1 +
             sig3
      sig3 = in' 2 ar A.outBus
      shp1 = env [0.02, 0.3, 0.02] [0.4, 0.01]
             (map EnvNum [3, -4]) 1 1
      gt = 1 - trig A.gate 0.01

dup :: UGen -> UGen
dup a = mce [a,a]

data Perc = Kick
          | Snare
          | Hat
          | Clap
            deriving (Eq, Show, Enum, Bounded)

instance Random Perc where
    random = chooseOne [minBound .. maxBound]
    randomR (min,max) = chooseOne [min .. max]

-- playRandom :: BPM -> IO ()
-- playRandom bpm = do
--   let durs = scanl (+) 0 $ repeat 0.25
--   oscs <- map percToOSC <$> randoms <$> newStdGen
--   spawn 0 bpm $ listE $ zip durs oscs

playPerc :: Perc -> IO ()
playPerc p = withSC3 $ (\fd -> send fd $ percToOSC p)

percToOSC :: Perc -> OSC
percToOSC = f . map toLower . show
    where f name = s_new name (-1) AddToTail 1 []

acidTree :: SCTree
acidTree =
  Group 0
    [Group 1
      [Group acidGroup
        [Synth bNodeId "acid" []],
       Group fxGroup
        [Synth fxNodeId "fx" []]]]

acidGroup :: Num a => a
acidGroup = 100

fxGroup :: Num a => a
fxGroup = 101

fxNodeId :: Num a => a
fxNodeId = 1002

bNodeId :: Num a => a
bNodeId = 1001

-- | Plays phrases. Try:
--
-- > > var1 <- newMVar dseq1
-- > > var2 <- newMVar bseq
-- > > var3 <- newMVar fseq
-- > > t1 <- forkIO (forever $ playSeqs 130 var1 var2 var3)
-- > > swapMVar var1 dseq2
-- > > swapMVar var2 =<< bseqG <$> newStdGen
-- > > killThread t1
--
-- playSeqs :: BPM
--          -> MVar (Map String [Double]) -- ^ MVar for dseq
--          -> MVar (Map String [Double]) -- ^ MVar for bseq
--          -> MVar (Map String [Double]) -- ^ MVar for fx
--          -> IO ()
-- playSeqs bpm ds bs fs = do
--   d <- readMVar ds
--   b <- readMVar bs
--   f <- readMVar fs
--   _ <- forkIO $ spawn 0 bpm $ dseqToE d
--   _ <- forkIO $ spawn 0 bpm $ bseqToE durs b
--   _ <- forkIO $ spawn 0 bpm $ fseqToE f
--   pauseThread (4*60/bpm)

-- ps :: BPM -> IO (ThreadId, [MVar (Map String [Double])])
ps bpm = do
  vd <- newMVar dseq0
  vb <- newMVar bseq
  vf <- newMVar fseq
  t1 <- forkIO (forever $ playSeqs bpm vd vb vf)
  return (t1,[vd,vb,vf])

type Rt = Map String [Double]

playSeqs :: Double -> MVar Rt -> MVar Rt -> MVar Rt -> IO ThreadId
playSeqs bpm d b f = do
  forever $ do
    threadDelay (floor $ ((1/4) * 60/bpm) * 1e6)
    print "foo"
  return undefined

playSeq :: Double -> MVar Rt -> IO ()
playSeq bpm m = do
  print "hit!"
  threadDelay (floor $ (60/bpm) * 1e6)

playTrack :: Double -> Rt -> IO ()
playTrack bpm m = mapM_ g os
  where
    g ps = do
      withSC3 $ \fd -> send fd $ Bundle (NTPi 1) (catMaybes ps)
      threadDelay (floor $ (1/4) * (60/bpm) * 1e6)
    os = transpose . M.elems . M.mapWithKey (\k v -> map (mkPerc' k) v) $ m

-- playTrack :: Double -> Rt -> IO ()
playTrack' m = do
  transpose . M.elems . M.mapWithKey f $ m
  where f k v = map (mkPerc' k) v

bseq :: Map String [Double]
bseq = M.fromList $
  [("gate",  [1,1,1,1, 1,1,1,1, 0,1,0,1, 1,1,1,0]),
   ("delta", [1,1,0,2, 1,1,0,0, 2,0,2,0, 1,2,0,4]),
   ("pitch", map (+38)
             [-24,-12,0,-12, 0,-12,10,12, 0,7,-7,0, -11,1,13,15])]

bseqG :: StdGen -> Map String [Double]
bseqG g = M.fromList $
  [("gate", take 16 $ choices [0,1,1,1] g1),
   ("delta", take 16 $ choices [0,1,2] g2),
   ("pitch", map (+38) $ concat
           [take 8 $ choices [-24,-12,-11,0,12] g3,
            take 4 $ choices [0,5,7,-5,-7] g4,
            take 4 $ choices [-11,1,13,15] g5])]
    where
      [g1,g2,g3,g4,g5] = take 5 $ iterate (snd . next) g

dseq0 :: Map String [Double]
dseq0 = M.fromList $
  [("kick",  [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0]),
   ("snare", [0,0,0,0, 4,0,0,2, 0,0,0,0, 4,0,0,0]),
   ("clap",  [0,0,0,0, 0,0,0,0, 0,0,0,0, 4,0,0,0]),
   ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])]

dseq1 :: Map String [Double]
dseq1 = M.fromList $
  [("kick",  [1,0,0,0, 0,0,0,0, 1,0,0,1, 0,0,1,0]),
   ("snare", [0,0,0,0, 0,0,0,2, 0,2,1,0, 4,3,3,3]),
   ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0]),
   ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])]

dseq2 :: Map String [Double]
dseq2 = M.fromList $
  [("kick",  [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0]),
   ("snare", [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0]),
   ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0]),
   ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])]

dseq3 :: Map String [Double]
dseq3 = M.fromList $
  [("kick",  [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0]),
   ("snare", [0,0,0,0, 0,0,0,2, 0,0,0,0, 0,0,0,0]),
   ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0]),
   ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2])]

dseq4 :: RandomGen g => g -> Map String [Double]
dseq4 g = M.fromList $
  [("kick",  take 16 $ choices [0,1] g0),
   ("snare", take 16 $ choices [0,1,2,4] g1),
   ("clap",  take 16 $ choices [0,4] g2),
   ("hat",   take 16 $ choices [0,1,2,4] g3)]
 where
   [g0,g1,g2,g3] = take 4 $ iterate (snd . next) g

fseq :: Map String [Double]
fseq = M.fromList
       [("gate", [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0])]

-- dseqToE :: Map String [Double] -> Event OSC
-- dseqToE ds = mconcat [k,s,c,h]
--     where
--       [k,s,c,h] = map f ["kick", "snare", "clap", "hat"]
--       ds' = M.update (return . map (*0.7)) "snare" .
--             M.update (return . map (*0.5)) "clap" .
--             M.update (return . map (*0.32)) "hat" $ ds
--       f name = mkEvent durs $
--                map (mkPerc' name) $ maybe [] id $ M.lookup name $ ds'

-- mkEvent :: [Double] -> [Maybe OSC] -> Event OSC
-- mkEvent durs oscs = listE $ catMaybes $ zipWith f durs oscs
--     where f a b = pure (,) <*> pure a <*> b

mkPerc' :: String -> Double -> Maybe OSC
mkPerc' name a
    | a > 0 = Just $ s_new name (-1) AddToTail acidGroup
              [("amp", squared (a/4))]
    | otherwise = Nothing

-- bseqToE :: [Double] -> Map String [Double] -> Event OSC
-- bseqToE ds ms = mconcat [ptc,gts,dlt]
--     where
--       ptc = listE $ zip ds $ mkNSet bNodeId $
--             M.filterWithKey (\k _ -> k == "pitch") ms
--       gts = listE $ catMaybes $ mkGates ds $ maybe [] id $
--             M.lookup "gate" ms
--       dlt = listE $ catMaybes $ mkDeltas ds $ maybe [] id $
--             M.lookup "delta" ms

mkGates :: [Double] -> [Double] -> [Maybe (Double, OSC)]
mkGates = zipWith f
    where
      f d n | n > 0 = Just $ (d, n_set bNodeId [("gate",n)])
            | otherwise = Nothing

mkDeltas :: [Double] -> [Double] -> [Maybe (Double, OSC)]
mkDeltas = zipWith f
    where
      f d dt | dt > 0 = Just $ (d+(dt/4)*0.99,
                                n_set bNodeId [("gate",0)])
             | otherwise = Nothing

-- fseqToE :: Map String [Double] -> Event OSC
-- fseqToE = listE . zip durs . mkNSet fxNodeId

durs :: [Double]
durs = scanl1 (+) $ cycle [0.28,0.22,0.27,0.23]
