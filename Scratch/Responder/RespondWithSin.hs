------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with a OSC message sent from scsynth, take 2.
--
-- Add sendTrig node in scsynth, and main action in this module will respond
-- with sending s_new OSC message.
--
-- Frequency for sin oscillator sound is updated from haskell side using MVar.
--
module RespondWithSin where

import Control.Concurrent
import Control.Monad

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID

main :: IO ()
main = withSC3 $ \fd -> do
  async fd $ notify True
  async fd $ d_recv $ synthdef "sinPerc" sinPerc
  async fd $ d_recv $ synthdef "nzPerc" nzPerc
  ivar <- newMVar 10000
  pvar <- newMVar 67
  forever $ do
    msg <- recv fd
    respond fd ivar pvar msg

sinPerc :: UGen
sinPerc = out 0 $ pan2 (sig * e) 0 1
  where
    sig = sinOsc ar (control kr "freq" 440) (-0.2) * 0.2
    e = xLine kr 1 1e-9 0.3 RemoveSynth

nzPerc :: UGen
nzPerc = out 0 $ pan2 (sig * e) 0.5 1
  where
    sig = resonz (whiteNoise 'a' ar) 8000 0.8 * 0.2
    e = envGen kr 1 0 1 1 RemoveSynth $ envPerc 5e-3 300e-3

respond :: (Transport t) => t -> MVar Int -> MVar Int -> OSC -> IO ()
respond fd ivar pvar (Message "/tr" [Int nid, Int tid, Float v]) = do
  i <- takeMVar ivar
  p <- fromIntegral `fmap` takeMVar pvar
  now <- utcr
  let pm = s_new "sinPerc" i AddToTail 1 [("freq",mkF p)]
      msg = if even p
            then Bundle (UTCr $ now + 0.1) [pm]
            else Bundle (UTCr $ now + 0.1) [pm, s_new "nzPerc" (i+1) AddToTail 1 []]
  send fd msg
  putMVar ivar (i+2)
  putMVar pvar (p+1)
respond _ _ _ _ = return ()

mkF :: (UnaryOp a) => Int -> a
mkF p = midiCPS $ ts  !! (p `mod` length ts)
  where ts = [60,67,64,65,59,84,70,79]