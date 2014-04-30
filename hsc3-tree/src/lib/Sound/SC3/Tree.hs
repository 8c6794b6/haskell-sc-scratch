{- |
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com

Stability   : unstable
Portability : portable

Representation of scsynth node tree.

-}
module Sound.SC3.Tree
  ( -- * Examples

    -- ** Interactive use
    -- $example_interactive

    -- ** Routing nodes
    -- $example_routing

    -- ** Querying
    -- $example_query

    -- * Module re-exports
    module Sound.SC3.Tree.Connection
  , module Sound.SC3.Tree.Diff
  , module Sound.SC3.Tree.Nd
  , module Sound.SC3.Tree.Type
  , module Sound.SC3.Tree.Query
  , module Sound.SC3.Tree.Zipper
  )  where

import Sound.SC3.Tree.Connection
import Sound.SC3.Tree.Diff
import Sound.SC3.Tree.Nd
import Sound.SC3.Tree.Type
import Sound.SC3.Tree.Query
import Sound.SC3.Tree.Zipper


{-$example_interactive

Dump the contents of running synth nodes from ghci:

>>> import Sound.SC3
>>> import Sound.SC3.Tree
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
@everywhere@ and @mkT@ from "Data.Generics" in syb package:

>>> import Data.Generics
>>> t <- withSC3 getRootNode
>>> let f (Synth i n ps) = Synth (2000 + abs i) "default" ps; f x = x
>>> withSC3 $ addNode 0 $ everywhere (mkT f) t

Doubling the frequencies:

>>> let g ("freq":=f) = "freq":=(f*2); g x = x
>>> withSC3 $ patchNode $ everywhere (mkT g) t
-}

{-$example_routing

Write node structure and send it to scsynth. \"fmod\" parameters
in synth \"bar\" are mapped from control rate outputs of synth \"foo\".

> import Sound.SC3
> import Sound.SC3.Tree
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

{-$example_query

Suppose that we have a 'SCNode' shown in routing example:

>>> n <- withSC3 getRootNode
>>> putStrLn $ drawSCNode n
0 group
   1 group
      10 group
         10000 foo
           amp: 100.0 freq: 0.6600000262260437 out: 100.0
         10001 foo
           amp: 80.0 freq: 3.3299999237060547 out: 101.0
      11 group
         11000 bar
           pan: 0.75 amp: 0.10000000149011612 freq: 220.0 fmod: c100 out: 0.0
         11001 bar
           pan: -0.75 amp: 0.10000000149011612 freq: 330.0 fmod: c101 out: 0.0

Querying a node in group 10 with 'nodeId' and '==?':

>>> let (g10:_) = queryN (nodeId ==? 10) n
>>> putStrLn $ drawSCNode g10
10 group
   10000 foo
     amp: 100.0 freq: 0.6600000262260437 out: 100.0
   10001 foo
     amp: 80.0 freq: 3.3299999237060547 out: 101.0

Querying nodes with condition to 'SynthParam' with 'params'. Filtering nodes
containing \"fmod\" parameter:

>>> let fmods = queryN (params (paramName ==? "fmod")) n
>>> mapM_ (putStrLn . drawSCNode) fmods
11000 bar
  pan: 0.75 amp: 0.10000000149011612 freq: 220.0 fmod: c100 out: 0.0
11001 bar
  pan: -0.75 amp: 0.10000000149011612 freq: 330.0 fmod: c101 out: 0.0

-}
