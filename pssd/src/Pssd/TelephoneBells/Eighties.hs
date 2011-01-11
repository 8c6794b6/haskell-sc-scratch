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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_phones.html>
--
-- Try:
--
-- > > withSC3 reset
-- > > audition fadeAM
-- > > audition ringing
-- > > dial
--
module Pssd.TelephoneBells.Eighties where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Random (randomRIO)
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import Pssd.Util

-- | Basic additive methods
-- Addiing two sine oscillators, with having 1234 hz and 789 hz frequencies.
basicAdditive :: UGen
basicAdditive = out 0 $ (o 1234 + o 789) * 0.2
  where o f = sinOsc ar f 0

-- | Synthdef for dial tone.
dtmfDialler :: UGen
dtmfDialler = out 0 $ pan2 (sig*0.2) 0 1
  where
    sig = amp * (anOsc "freq1" 697 + anOsc "freq2" 1209)
    anOsc name val = sinOsc ar (ctrl name val) 0
    amp = envGen kr trg 1 0 1 DoNothing shape
    trg = ctrl "t_trig" 0
    shape = envLinen 0.01 0.2 0.01 1

-- | Lookup table for button to frequencies
buttonTable :: M.Map Char (Double, Double)
buttonTable = M.fromList $ zip chars freqs
  where
    chars = "123A456B789C*0#D"
    freqs = zip lows highs
    lows = concat $ map (replicate 4) [697,770,852,941]
    highs = concat $ replicate 4 [1209, 1336, 1477, 1633]

-- | Get char from stdin and push the button.
dial :: IO ()
dial = withSC3 $ \fd -> do
  async fd $ d_recv $ synthdef "dtmfDialler" dtmfDialler
  send fd $ s_new "dtmfDialler" (-1) AddToTail 1 []
  send fd $ s_get (-1) []
  (Message _ (Int i:_)) <- wait fd "/n_set"
  forever (go fd i)
  where
    go fd nid = do
      c <- getChar
      del <- randomRIO (0.2,0.5::Double)
      case M.lookup c buttonTable of
        Nothing      -> return ()
        Just (f1,f2) -> send fd $
          n_set nid [ ("t_trig", 1)
                    , ("freq1", f1)
                    , ("freq2", f2) ]
      threadDelay (floor $ del * 1000 * 1000)

-- | Fading between am and mere addition of 2 sinosc.
-- Mouse x controls the amplitude balance between the two.
fadeAM :: UGen
fadeAM = out 0 $ pan2 (sig * 0.2) 0 1
  where
    sig = (amount * addO) + (1 - amount) * mulO
    addO = osc1 + osc2
    mulO = osc1 * osc2
    osc1 = sinOsc ar 1234 0
    osc2 = sinOsc ar 789 0
    amount = mouseX kr 0 1 Linear 0.1

-- | Ringing sound, shown as amphonetone.jpg.
--
-- Note that to make inharmonic sound, frequencies used for am oscillators are
-- coprime to each others.
--
ringing :: UGen
ringing = out 0 $ pan2 (sig * 0.2) 0 1
  where
    sig = modulated * onOff
    modulated = foldr (\a b -> f a * b) 1 [1112, 419, 173, 13]
    f freq = sinOsc ar freq 0
    onOff = clip (f 0.2 * 100) 0 1