------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Representation of scsynth node tree.
--
module Sound.SC3.Lepton.Tree
  ( -- * Usage Examples

    -- ** Quick interaction
    -- $example_interactive

    -- ** Routing nodes
    -- $example_declarative
    module Sound.SC3.Lepton.Tree.Connection
  , module Sound.SC3.Lepton.Tree.Diff
  , module Sound.SC3.Lepton.Tree.Nd
  , module Sound.SC3.Lepton.Tree.Tree
  , module Sound.SC3.Lepton.Tree.Zipper
  )  where

import Sound.SC3.Lepton.Tree.Connection
import Sound.SC3.Lepton.Tree.Diff
import Sound.SC3.Lepton.Tree.Nd
import Sound.SC3.Lepton.Tree.Tree
import Sound.SC3.Lepton.Tree.Zipper

-- $example_interactive
--
-- Dump the contents of running synth nodes from ghci:
--
-- > > :m + Sound.SC3
-- > > withSC3 reset
-- > > withSC3 printRootNode
-- > 0 group
-- >    1 group
--
-- Dump again after adding synth nodes:
--
-- > > audition $ out 0 $ sinOsc ar (control kr "freq" 440) 0 * 0.3
-- > > audition $ out 0 $ sinOsc ar (control kr "freq" 330) 0 * 0.3
-- > > withSC3 printRootNode
-- > 0 group
-- >    1 group
-- >       -16 Anonymous
-- >         freq: 440.0
-- >       -24 Anonymous
-- >         freq: 330.0
--
-- Updating synth nodes with using generic transforming function, e.g.
-- @transform@ from uniplate package:
--
-- > > :m + Data.Generics.Uniplate.Data
-- > > t <- withSC3 getRootNode
-- > > let f (Synth i n ps) = Synth (2000 + abs i) "default" ps; f x = x
-- > > withSC3 $ addNode 0 $ transform f t
--
-- Using @transformBi@:
--
-- > > let g ("freq":=f) = "freq":=(f*2); g x = x
-- > > withSC3 reset
-- > > withSC3 $ addNode 0 $ transformBi g t
--

-- $example_declarative
--
-- Write node structure and send it to scsynth. \"fmod\" parameters
-- in synth \"bar\" are mapped from control rate outputs of synth \"foo\".
--
-- > import Sound.OpenSoundControl
-- > import Sound.SC3
-- > import Sound.SC3.Lepton
-- >
-- > main :: IO ()
-- > main = withSC3 playFooBar
-- >
-- > playFooBar :: (Transport t) => t -> IO ()
-- > playFooBar fd = do
-- >   mapM (\(n,u) -> async fd . d_recv $ synthdef n u)
-- >     [("foo",foo),("bar",bar)]
-- >   addNode 0 nodes fd
-- >
-- > nodes :: SCNode
-- > nodes =
-- >   Group 1
-- >     [Group 10
-- >       [Synth 1000 "foo"
-- >          ["out":=100,"amp":=100,"freq":=1.66]
-- >       ,Synth 1001 "foo"
-- >          ["out":=101,"amp":=80,"freq":=3.33]]
-- >     ,Group 11
-- >       [Synth 1100 "bar"
-- >          ["amp":=0.1,"pan":=0.5,"freq":=110,"fmod":<-100]
-- >       ,Synth 1101 "bar"
-- >          ["amp":=0.1,"pan":=(-0.5),"freq":=330,"fmod":<-101]]]
-- >
-- > foo :: UGen
-- > foo = out outBus (sinOsc kr freq 0 * amp)
-- >
-- > bar :: UGen
-- > bar = out 0 $ pan2 (saw ar (fmod + freq) * amp) pan 1
-- >
-- > outBus, amp, freq, pan, fmod :: UGen
-- > outBus = control kr "out" 0
-- > amp = control kr "amp" 0.3
-- > freq = control kr "freq" 440
-- > pan = control kr "pan" 0
-- > fmod = control kr "fmod" 0
