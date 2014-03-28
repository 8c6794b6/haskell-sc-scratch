{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Example showing sequence of actions and declarative node routing with functions
from "Sound.SC3.Tree" module.

-}
module Main where

import Sound.SC3
import Sound.SC3.Tree

import Data.Generics (everywhere, mkT)


-- --------------------------------------------------------------------------
--
-- Interactions
--
-- --------------------------------------------------------------------------

-- | Printing the default setting done by 'reset'.
act01 :: IO ()
act01 = do
  withSC3 reset
  withSC3 printRootNode

-- | Printing the root node after adding two anonymous scythes.
act02 :: IO ()
act02 = do
  audition $ out 0 $ sinOsc AR (control KR "freq" 440) 0 * 0.1
  audition $ out 0 $ sinOsc AR (control KR "freq" 330) 0 * 0.1
  withSC3 printRootNode

-- | Assuming that the synth nodes added with 'act02' are running.
act03 :: IO ()
act03 = do
  t <- withSC3 getRootNode
  let f (Synth i _ ps) = Synth (2000 + abs i) "default" ps; f x = x
  withSC3 $ addNode 0 $ everywhere (mkT f) t

-- | Assuming that the synth nodes transformed with 'act03' are running.
act04 :: IO ()
act04 = do
  t <- withSC3 getRootNode
  let g ("freq":=f) = "freq":=(f*2); g x = x
  withSC3 $ patchNode $ everywhere (mkT g) t


-- --------------------------------------------------------------------------
--
-- Node routing example
--
-- --------------------------------------------------------------------------

main :: IO ()
main = withSC3 $  do
  mapM_ (async . d_recv . uncurry synthdef) [("foo",foo),("bar",bar)]
  patchNode $ nodify nodes

nodes :: Nd
nodes =
  let mod1 = syn "foo" ["out"*=100, "amp"*=100, "freq"*=0.66]
      mod2 = syn "foo" ["out"*=101, "amp"*=80, "freq"*=3.33]
  in grp 0
    [ grp 1
      [ grp 10
        [ mod1, mod2 ]
      , grp 11
        [ syn "bar"
          [ "amp"*=0.1, "pan"*=0.75, "freq"*=220, "fmod"*<-mod1-*"out"]
        , syn "bar"
          [ "amp"*=0.1, "pan"*=(-0.75), "freq"*=330, "fmod"*<-mod2-*"out"] ]]]

foo :: UGen
foo = out outBus (sinOsc KR freq 0 * amp)

bar :: UGen
bar = out outBus $ pan2 (saw AR (fmod + freq) * amp) pan 1

outBus, amp, freq, pan, fmod :: UGen
outBus = control KR "out" 0
amp    = control KR "amp" 0.3
freq   = control KR "freq" 440
pan    = control KR "pan" 0
fmod   = control KR "fmod" 0

-- --------------------------------------------------------------------------
--
-- Querying example
--
-- --------------------------------------------------------------------------

-- | Assuming that 'main' action has executed.
act05 :: IO ()
act05 = do
  n <- withSC3 getRootNode
  putStrLn $ drawSCNode n

act06 :: IO ()
act06 = do
  n <- withSC3 getRootNode
  let (g10:_) = queryN (nodeId ==? 10) n
  putStrLn $ drawSCNode g10

act07 :: IO ()
act07 = do
  n <- withSC3 getRootNode
  let fmods = queryN (params (paramName ==? "fmod")) n
  mapM_ (putStrLn . drawSCNode) fmods
