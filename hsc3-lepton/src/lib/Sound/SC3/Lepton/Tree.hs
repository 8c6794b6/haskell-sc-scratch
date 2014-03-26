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

    -- * Module re-exports
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

{-$example_interactive

Dump the contents of running synth nodes from ghci:

>>> import Sound.SC3
>>> import Sound.SC3.Lepton.Tree
>>> withSC3 reset
>>> withSC3 printRootNode
0 group
   1 group
   2 group

Dump again after adding synth nodes:

>>> audition $ out 0 $ sinOsc AR (control KR "freq" 440) 0 * 0.1
>>> audition $ out 0 $ sinOsc AR (control KR "freq" 330) 0 * 0.1
>>> withSC3 printRootNode
0 group
   1 group
      -16 Anonymous
        freq: 440.0
      -24 Anonymous
        freq: 330.0
   2 group

Updating synth nodes with using generic transforming function, e.g.
'transform' from uniplate package:

>>> import Data.Generics.Uniplate.Data
>>> t <- withSC3 getRootNode
>>> let f (Synth i n ps) = Synth (2000 + abs i) "default" ps; f x = x
>>> withSC3 $ addNode 0 $ transform f t

Using @transformBi@:

>>> let g ("freq":=f) = "freq":=(f*2); g x = x
>>> withSC3 $ patchNode $ transformBi g t
-}

{-$example_declarative

Write node structure and send it to scsynth. \"fmod\" parameters
in synth \"bar\" are mapped from control rate outputs of synth \"foo\".

> import Sound.SC3
> import Sound.SC3.Lepton.Tree
>
>
> main :: IO ()
> main = withSC3 $  do
>   mapM (async . d_recv . uncurry synthdef)
>     [("foo",foo),("bar",bar)]
>   patchNode $ nodify nodes
>
> nodes :: Nd
> nodes =
>   let mod1 = syn "foo" ["out"*=100, "amp"*=100, "freq"*=0.66]
>       mod2 = syn "foo" ["out"*=101, "amp"*=80, "freq"*=3.33]
>   in grp 0
>     [ grp 1
>       [ grp 10
>         [ mod1, mod2 ]
>       , grp 11
>         [ syn "bar"
>           [ "amp"*=0.1, "pan"*=0.5, "freq"*=220, "fmod"*<-mod1-*"out"]
>         , syn "bar"
>           [ "amp"*=0.1, "pan"*=(-0.5), "freq"*=330, "fmod"*<-mod2-*"out"]]]]
>
> foo :: UGen
> foo = out outBus (sinOsc KR freq 0 * amp)
>
> bar :: UGen
> bar = out 0 $ pan2 (saw AR (fmod + freq) * amp) pan 1
>
> outBus, amp, freq, pan, fmod :: UGen
> outBus = control KR "out" 0
> amp = control KR "amp" 0.3
> freq = control KR "freq" 440
> pan = control KR "pan" 0
> fmod = control KR "fmod" 0

-}
