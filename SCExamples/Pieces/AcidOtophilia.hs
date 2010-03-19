------------------------------------------------------------------------------
-- |
-- Module      : SCExamples.Pieces.AcidOtopholia
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing sc3 example code from
-- /acid_otophilia.scd/.
--
--

module SCExamples.Pieces.AcidOtophilia where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Data.Map (Map)
import System.Random

import qualified Data.Map as M

import FRP.Reactive
import Sound.OpenSoundControl
import Sound.SC3

import Reusable
import SCSched
import SCTree
import SCQuery

runAcidOtophilia :: IO ()
runAcidOtophilia = do
  undefined

setAcidOtophilia :: IO ()
setAcidOtophilia = withSC3 $ \fd -> do
  mkTree acidTree fd
  -- zipWithM (lsd fd)
  --    ["kick", "snare", "clap", "hat", "acid"]
  --    [kick, snare, clap, hat, acid]
  --   where
  --     lsd fd name ioUGen =
  --         ioUGen >>= \ug -> loadSynthdef name ug fd

kick :: IO UGen
kick = undefined

snare :: IO UGen
snare = undefined

clap :: IO UGen
clap = undefined

hat :: IO UGen
hat = undefined

acid :: IO UGen
acid = undefined

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

playRandom :: BPM -> IO ()
playRandom bpm = do
  let durs = scanl (+) 0 $ repeat 0.25
  oscs <- map percToOSC <$> randoms <$> newStdGen
  spawn 0 bpm $ listE $ zip durs oscs

percToOSC :: Perc -> OSC
percToOSC = f . map toLower . show
    where f name = s_new name (-1) AddToTail acidGroup []

runDseq :: MVar (Map String [Double]) -> IO (Event OSC)
runDseq var = do
  undefined

runBseq :: IO ()
runBseq = do
  let es = undefined
  spawn 0 130 es

runFx :: MVar (Map String [Double]) -> IO (Event OSC)
runFx var = do
  undefined

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
   ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2]),
   ("fx",    [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0])]

dseq1 :: Map String [Double]
dseq1 = M.fromList $
  [("kick",  [1,0,0,0, 0,0,0,0, 1,0,0,1, 0,0,1,0]),
   ("snare", [0,0,0,0, 0,0,0,2, 0,2,1,0, 4,3,3,3]),
   ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0]),
   ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2]),
   ("fx",    [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0])]

dseq2 :: Map String [Double]
dseq2 = M.fromList $
  [("kick",  [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0]),
   ("snare", [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0]),
   ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0]),
   ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2]),
   ("fx",    [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0])]

dseq3 :: Map String [Double]
dseq3 = M.fromList $
  [("kick",  [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0]),
   ("snare", [0,0,0,0, 0,0,0,2, 0,0,0,0, 0,0,0,0]),
   ("clap",  [0,0,0,0, 4,0,0,0, 0,0,0,0, 4,0,0,0]),
   ("hat",   [1,2,4,0, 1,0,4,0, 1,2,4,2, 1,0,4,2]),
   ("fx",    [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0])]

dseq4 :: RandomGen g => g -> Map String [Double]
dseq4 g = M.fromList $
  [("kick",  take 16 $ choices [0,1] g0),
   ("snare", take 16 $ choices [0,1,2,4] g1),
   ("clap",  take 16 $ choices [0,4] g2),
   ("hat",   take 16 $ choices [0,1,2,4] g3),
   ("fx",    take 16 $ choices [0,1] g4)]
 where
   [g0,g1,g2,g3,g4] = take 5 $ iterate (snd . next) g

fseq :: Map String [Double]
fseq = M.fromList 
       [("gate", [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0])]

dseqToE :: Map String [Double] -> Event OSC
dseqToE ds = mconcat [k,s,c,h]
    where
      [k,s,c,h] = map f ["kick", "snare", "clap", "hat"]
      ds' = M.update (return . map (*0.7)) "snare" .
            M.update (return . map (*0.5)) "clap" .
            M.update (return . map (*0.32)) "hat" $ ds
      f name = mkEvent durs $
               map (mkPerc name) $ maybe [] id $ M.lookup name $ ds'

mkEvent :: [Double] -> [Maybe OSC] -> Event OSC
mkEvent durs oscs = listE $ catMaybes $ zipWith f durs oscs
    where f a b = pure (,) <*> pure a <*> b

mkPerc :: String -> Double -> Maybe OSC
mkPerc name amp =
  if amp > 0
     then Just $ s_new name (-1) AddToTail acidGroup
              [("amp", squared (amp/4))]
     else Nothing

bseqToE :: [Double] -> Map String [Double] -> Event OSC
bseqToE ds ms = mconcat [ptc,gts,dlt]
    where
      ptc = listE $ zip ds $ mkNSet bNodeId $
            M.filterWithKey (\k _ -> k == "pitch") ms
      gts = listE $ catMaybes $ mkGates ds $ maybe [] id $
            M.lookup "gate" ms
      dlt = listE $ catMaybes $ mkDeltas ds $ maybe [] id $
            M.lookup "delta" ms

mkGates :: [Double] -> [Double] -> [Maybe (Double, OSC)]
mkGates = zipWith f
    where
      f d 1 = Just $ (d, n_set bNodeId [("gate", 1)])
      f d _ = Nothing

mkDeltas :: [Double] -> [Double] -> [Maybe (Double, OSC)]
mkDeltas = zipWith f
    where
      f d dt | dt > 0 = Just $ (d+(dt/4)*0.99,
                                n_set bNodeId [("gate",0)])
             | otherwise = Nothing

playDseq :: BPM -> MVar (Map String [Double]) -> IO ()
playDseq bpm var = do
  m <- readMVar var
  _ <- forkIO $ spawn 0 bpm $ dseqToE m
  pauseThread (4*60/bpm)


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
playSeqs :: BPM
         -> MVar (Map String [Double]) -- ^ MVar for dseq
         -> MVar (Map String [Double]) -- ^ MVar for bseq
         -> MVar (Map String [Double]) -- ^ MVar for fx
         -> IO ()
playSeqs bpm ds bs fs = do
  d <- readMVar ds
  b <- readMVar bs
  f <- readMVar fs 
  _ <- forkIO $ spawn 0 bpm $ dseqToE d
  _ <- forkIO $ spawn 0 bpm $ bseqToE durs b
  _ <- forkIO $ spawn 0 bpm $ fseqToE f
  pauseThread (4 * 60 / bpm)

fseqToE :: Map String [Double] -> Event OSC
fseqToE = listE . zip durs . mkNSet fxNodeId

durs :: [Double]
durs = scanl1 (+) $ cycle [0.29,0.21,0.27,0.23]

loopEvent :: BPM -> MVar (Event OSC) -> IO ThreadId
loopEvent bpm var = forkIO (forever g)
    where
      g = do
        ev <- readMVar var
        spawn 0 bpm ev
