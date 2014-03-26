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
    module All
  ) where

import Sound.SC3.Lepton.Instance ()
import Sound.SC3.Lepton.Looper as All
import Sound.SC3.Lepton.Pattern as All
import Sound.SC3.Lepton.UGen as All
import Sound.SC3.Lepton.Util as All

{-$overview

hsc3-lepton package contains client side utility collection for
working with SuperCollider's synthesis engine, with heavily depending
on hsc3 and hosc packages.

This module re-exports most modules in hsc3-lepton packages, except for
Parsers. When any functions in "Sound.SC3.Lepton.Parser" is needed, import them
individually.

"Sound.SC3.Lepton.Pattern" contains functions to compose patterns.
Patterns could be used for expressing sequential audible events in higher level.

"Sound.SC3.Lepton.Parser" contains parsers for OSC datum message and
supercollider synth definition file format. To read the contents of synthdef
file, use 'parseSynthDefFile'.

-}
