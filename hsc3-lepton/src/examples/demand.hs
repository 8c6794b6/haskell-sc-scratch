{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Example showing the usage of demand ugen builder functions from
"Sound.SC3.Lepton.UGen.Demand" module, with rewriting example from
streams-patterns-event tutorial in SC3 help file.

-}
module Main where

import System.Random

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton.UGen.Demand

main :: IO ()
main = audition $ playS sup

playS :: Supply -> UGen
playS sp = out 0 sig
  where
    sig   = foldr f v (zipWith mce2 (mkRs "abcd") (mkRs "efgh"))
    f a b = allpassN b 0.05 a 4
    v     = rlpf (pulse AR (mce2 freq (freq*1.01)) bw) rq cf * amp
    rq    = lfdNoise3 'n' KR 2.323 * 2000 + 2200
    cf    = lfdNoise3 'q' KR 1.110 * 0.498 + 0.5
    bw    = lfdNoise3 'b' KR 0.1123 * 0.48 + 0.5
    mkRs  = map (\x -> rand x 0.001 0.1)
    freq  = midiCPS $ demand tick 0 (evalSupply sp (mkStdGen 0x81aafad))
    amp   = decay2 tick 5e-4 950e-3 * 0.2
    tick  = impulse KR 7.6923 0

sup :: Supply
sup =
  sseq sinf
  [srand 1
   [snil, sseq 1 [24,31,36,43,48,55]]
  ,sseq (siwhite sinf 2 5)
   [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
  ,srand (siwhite sinf 3 9)
   [74,75,77,79,81]]
