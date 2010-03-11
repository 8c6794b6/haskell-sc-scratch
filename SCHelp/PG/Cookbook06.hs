------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook06
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook06_Phrase_Network/.
--

module SCHelp.PG.Cookbook06 where

import Control.Applicative
import Control.Monad.State
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

-- | UGen for pharase network example.
sawpulse :: UGen
sawpulse = undefined

-- | BPM tempo.
phraseTempo :: Num a => a
phraseTempo = 128

-- | Type synonym for phrase.
type Phrase = Map String [Double]

type PhraseIndex = Int

type PhraseMatrix = [(PhraseIndex, [PhraseIndex])]

data PhraseState = PhraseState 
    { phraseSeed :: StdGen,
      phraseIndex :: PhraseIndex }

runPhrases :: IO ()
runPhrases = playPhrases =<< genPhrases <$> initPhraseState

setPhrase :: IO ()
setPhrase = withSC3 $ \fd -> do
  send fd $ s_new "sawpulse" phraseNodeId AddToTail 1 []             

cleanPhrase = withSC3 $ \fd -> do
  send fd $ n_free [phraseNodeId]              

genPhrases :: PhraseState -> [Phrase]
genPhrases = evalState (sequence $ repeat phraseDelta)

initPhraseState :: IO PhraseState
initPhraseState = fmap (\g -> PhraseState g 0) newStdGen

phraseDelta :: State PhraseState Phrase
phraseDelta = do
  st <- get
  let gen = phraseSeed st
      canditates = maybe [] id $ lookup (phraseIndex st) phraseMatrix
      (idx, gen') = chooseOne canditates gen
  put $ PhraseState gen' idx
  return $ phrases !! idx

phraseNodeId :: NodeId
phraseNodeId = 1001
  
playPhrases :: [Phrase] -> IO ()
playPhrases = mapM_ playPhrase

playPhrase :: Phrase -> IO ()
playPhrase p = do
  let p' = M.update (return . map midiCPS) "freq" p
      dur = scanl (+) 0 . maybe [] id . M.lookup "dur" $ p'
      ev = listE $ zip dur $ mkNSet phraseNodeId p'
  spawn 0 phraseTempo ev

phraseMatrix :: PhraseMatrix
phraseMatrix = 
    [(0, [0,1,2]),
     (1, [0]),
     (2, [1,2])]

phrases :: [Phrase]
phrases = [phr01,
           phr02,
           phr03]

phr01 :: Phrase
phr01 = M.fromList $
           [("freq",[78,81,78,76,78,76,72,71,69,66]),
            ("dur",[0.25,1.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25]),
            ("sustain", [0.3,1.2,0.3,0.2,0.3,0.2,0.3,0.2,0.3,0.2]),
            ("amp", [1,0.5,0.75,0.5,0.75,0.5,0.75,0.5,0.75,0.5]),
            ("mw", [0,0.03] ++ replicate 8 0)]

phr02 :: Phrase
phr02 = M.fromList $ 
        [("freq", [64, 66, 69, 71, 72, 73]),
         ("dur", replicate 6 0.25),
         ("sustain", [0.3, 0.2, 0.3, 0.3, 0.3, 0.2]),
         ("amp", [1, 0.5, 0.5, 0.5, 0.5, 0.5]),
         ("mw", replicate 6 0)]

phr03 :: Phrase
phr03 = M.fromList $ 
        [("freq", [69, 71, 69, 66, 64, 69, 71, 69]),
         ("dur", [0.125, 0.625, 0.25, 0.25, 0.25, 0.25, 0.25, 0.75]),
         ("sustain", [0.2, 0.64, 0.2, 0.2, 0.2, 0.3, 0.3, 0.75]),
         ("amp", [0.5, 0.75, 0.5, 0.5, 0.5, 1, 0.5, 0.5]),
         ("mw", replicate 8 0)]
         
