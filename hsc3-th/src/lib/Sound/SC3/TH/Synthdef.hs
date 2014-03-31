{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Extract and generate synthdefs from Haskell module.

-}
module Sound.SC3.TH.Synthdef where

import Language.Haskell.TH (ExpQ)
import Language.Haskell.Extract (functionExtractorMap)

import Sound.SC3 (synthdef)

{-|
Looks for functions having \"synth_\" prefix in current module and returns
a list of 'Synthdef's.

In moudle \"MySynthDefs.hs\":

> module MySynthDefs where
>
> import Sound.SC3
>
> synth_foo :: UGen
> synth_foo = out 0 (sinOsc AR 440 * 0.1)
>
> synth_bar :: UGen
> synth_bar = out 0 (saw AR 440)
>
> synthdefs :: [Synthdef]
> synthdefs = $(synthdefGenerator)

Above /synthdefs/ is equivalent of:

> synthdefs :: [Synthdef]
> synthdefs = [synthdef "foo" synth_foo, synthdef "bar" synth_bar]

-}
synthdefGenerator :: ExpQ
synthdefGenerator =
    functionExtractorMap "^synth_" [|\n ug -> synthdef (drop 6 n) ug|]
