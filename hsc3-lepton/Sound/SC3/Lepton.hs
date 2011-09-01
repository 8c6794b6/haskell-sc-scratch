------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Client side utility collection for scsynth.
--
module Sound.SC3.Lepton
  ( -- * Overview
    -- $overview

    -- * Node Mapping
    -- $node_mapping

    -- * Patterns
    -- $patterns

    -- * Parsers
    -- $parsers

    module All
  ) where

import Sound.SC3.Lepton.Instance ()
import Sound.SC3.Lepton.Looper as All
import Sound.SC3.Lepton.Pattern as All
import Sound.SC3.Lepton.Tree as All
import Sound.SC3.Lepton.UGen as All
import Sound.SC3.Lepton.Util as All

{-$overview

hsc3-lepton package contains client side utility collection for
working with SuperCollider's synthesis engine, with heavily depending
on hsc3 and hosc packages.

This module re-exports most modules in hsc3-lepton packages, except for
Parsers. When any functions in "Sound.SC3.Lepton.Parser" is needed, import them
individually.

-}

{-$node_mapping

"Sound.SC3.Lepton.Tree" module contains functions to map synth and groups in
declarative style. See "Sound.SC3.Lepton.Tree" for example usage.

-}

{-$patterns

"Sound.SC3.Lepton.Pattern" contains functions to compose patterns. See
"Sound.SC3.Lepton.Pattern" for example usage.

-}


{-$parsers

The package contains parsers for OSC datum message and supercollider synth
definition file format. To read the contents of synthdef file, use
'parseSynthDefFile'.

-}