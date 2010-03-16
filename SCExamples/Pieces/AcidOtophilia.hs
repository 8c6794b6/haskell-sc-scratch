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

setAcidOtophilia :: IO [OSC]
setAcidOtophilia = withSC3 $ \fd -> do
  zipWithM (lsd fd)
     ["kick", "snare", "clap", "hat", "acid"]
     [kick, snare, clap, hat, acid]
    where
      lsd fd name ioUGen =
          ioUGen >>= \ug -> loadSynthdef name ug fd

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
    where
      f name = s_new name (-1) AddToTail 1 []

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

runDseqMap :: Map String [Double] -> [Maybe OSC]
runDseqMap m = undefined

bNodeId :: Num a => a
bNodeId = 1001

bseq :: Map String [Double]
bseq = M.fromList $
  [("gate",  [1,1,1,1, 1,1,1,1, 0,1,0,1, 1,1,1,0]),
   ("delta", [1,1,0,2, 1,1,0,0, 2,0,2,0, 1,2,0,4]),
   ("pitch", map (+38) 
             [-24,-12,0,-12, 0,-12,10,12, 0,7,-7,0, -11,1,13,15])]

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

testDseq :: BPM -> Map String [Double] -> IO ()
testDseq bpm ds = spawn 0 bpm es
    where
      es = mconcat [k, s, c, h]
      f name = mkEvent durs $
               map (mkPerc name) $ maybe [] id $ M.lookup name $ ds
      [k,s,c,h] = map f ["kick", "snare", "clap", "hat"]

-- | Plays dseq. Try:
-- 
-- > > var <- newMVar dseq1
-- > > t1 <- forkIO (forever $ playDseq 130 var)
-- > > swapMVar var dseq2
-- > > swapMVar var dseq3
--
playDseq :: BPM -> MVar (Map String [Double]) -> IO ()
playDseq bpm var = do
  m <- readMVar var
  forkIO $ testDseq bpm m
  pauseThread (4 * 60 / bpm)

durs :: [Double]
durs = scanl (+) 0 $ cycle [0.29,0.21,0.27,0.23]

mkEvent :: [Double] -> [Maybe OSC] -> Event OSC
mkEvent durs oscs = listE $ catMaybes $ zipWith f durs oscs
    where f a b = pure (,) <*> pure a <*> b

mkPerc :: String -> Double -> Maybe OSC
mkPerc name amp =
  if amp > 0
     then Just $ s_new name (-1) AddToTail 1 $ [("amp", squared (amp/4))]
     else Nothing

