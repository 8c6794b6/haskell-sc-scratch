------------------------------------------------------------------------------
-- |
-- 
-- Exercise from /Stream-Patterns-Events2/.
--
-- 

module SCHelp.SPE.Part2 where

import Control.Applicative
import Control.Applicative.State
import Control.Monad.State
import Data.Monoid
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Wing
import qualified Sound.SC3.Wing.ControlArg as A

runPart2 :: IO ()
runPart2 = undefined

-- | Load synthdef used in this example.
setPart2 :: Transport t => t -> IO OSC
setPart2 fd = do
  ug <- help_SPE2
  loadSynthdef "help_SPE2" ug fd

-- | Synthdef for part2.
help_SPE2 :: IO UGen
help_SPE2 = out A.i_out <$> (mkRev <$> source <*> delays)
    where
      mkRev :: UGen -> [UGen] -> UGen
      mkRev v ds = foldr f v ds where f a b = allpassN b 0.05 a 4

      source :: IO UGen
      source = mkSig <$> (midiCPS . (+110) . (*36)) <$> lfNoise1 kr 1

      mkSig :: UGen -> UGen
      mkSig ug = rlpf (lfSaw ar A.freq 0) ug 0.1 * 
                 envGen kr 1 0.3 0 A.sustain RemoveSynth (envPerc 0.001 1)

      delays :: IO [UGen]
      delays = take 4 <$> 
               (zipWith mce2 <$> 
                (randomRs (0,0.05) <$> newStdGen) <*>
                (randomRs (0,0.05) <$> newStdGen))

mk_help_SPE2 :: Double -> Double -> OSC
mk_help_SPE2 s n = s_new "help_SPE2" (-1) AddToTail 1 
                   [("sustain",s),("freq",midiCPS n)]

type Rtn2 a = State StdGen a

higherDurs :: [Double]
higherDurs = repeat (1/7)

lowerDurs :: [Double]
lowerDurs = map (*3) higherDurs 

updateGen :: Rtn2 StdGen
updateGen = get >>= \g -> put (snd . next $ g) >> return g

randomReplicate :: (Int,Int) -> Rtn2 [a] -> Rtn2 [a]
randomReplicate range rtn = do
  g <- updateGen
  concat <$> replicateM (fst . randomR range $ g) rtn

events :: IO (Event OSC)
events = mappend <$> 
         (rtnToE lowerDurs lowerRtn) <*> 
         (rtnToE higherDurs higherRtn)

rtnToE :: [Double] -> Rtn2 [Double] -> IO (Event OSC)
rtnToE durs r = listE . zip durs' <$> (zipWith mk_help_SPE2 durs <$> oscs)
    where
      durs' = scanl (+) 0 durs
      oscs = evalState (concat <$> (sequence $ repeat r)) <$> newStdGen

higherRtn :: Rtn2 [Double]
higherRtn = concat <$> sequence [rtn1,rtn2,rtn3]

lowerRtn :: Rtn2 [Double]
lowerRtn = f <$> updateGen <*> higherRtn
    where f g ds = zipWith (-) ds (choices [0,7,7,12] g)

rtn1 :: Rtn2 [Double]
rtn1 = (fst . chooseOne [[],[24,31,36,43,48,55]]) <$> updateGen

rtn2 :: Rtn2 [Double]
rtn2 = randomReplicate (2,5) rtn2'
  
rtn2' :: Rtn2 [Double]
rtn2' = do
  g <- updateGen
  return $ 
       60 : (fst . chooseOne [63,65] $ g) : 67 : 
       (fst . chooseOne [70,72,74] $ g) : []

rtn3 :: Rtn2 [Double]
rtn3 = randomReplicate (3,9) rtn3'

rtn3' :: Rtn2 [Double]
rtn3' = return . fst . chooseOne  [74,75,77,79,81] <$> updateGen
