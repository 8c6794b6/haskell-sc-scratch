------------------------------------------------------------------------------
-- | Scratch to play with @sendTrig@ ugen.
--

module Scratch.SendingTrigger where

import Control.Monad (forever)

import Sound.OpenSoundControl
import Sound.SC3

import SCTree

main :: IO ()
main = withSC3 $ \fd -> do
         send fd (notify True)
         wait fd "/done"
         play fd =<< sendTriggeringUGen
         forever $ wait fd "/tr" >>= putStrLn . show

sendTriggeringUGen :: IO UGen
sendTriggeringUGen = do
  s <- lfNoise0 kr 10
  let o = sinOsc ar (s * 220 + 500) 0 * 0.1
  return $ mrg [sendTrig s 0 s, out 0 o]
