------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
module Test.Sound.SC3.Lepton.Parser where

import Control.Applicative
import Data.Word
import Test.QuickCheck hiding (Result)

import Data.Attoparsec (Result(..), parse)
import Data.ByteString (pack)
import Sound.OpenSoundControl
import Sound.SC3

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Sound.SC3.Lepton.Parser
import Sound.SC3.Lepton.QuickCheck

tests :: [Property]
tests = [label "parse_datum" prop_parse_datum
        ,label "parse_any_datum" prop_parse_any_datum
        ,label "parse_synthdef" prop_parse_synthdef]

prop_parse_datum :: Property
prop_parse_datum =
  forAll (arbitrary `suchThat` (not . null)) $ \ds ->
  either (const False) (const True) (parseDatum datum ds)

prop_parse_any_datum :: Property
prop_parse_any_datum =
  forAll (arbitrary `suchThat` (not . null)) $ \ds ->
  let p f = case parseDatum f ds of Right _ -> True; _ -> False
  in  p int .||. p float .||. p double .||. p string .||.
      p blob .||. p timeStamp .||. p midi

prop_parse_synthdef :: Property
prop_parse_synthdef =
  forAll (arbitrary `suchThat` \(dn,cn,v) ->
           dn /= [] && cn /= []) $ \(dn,cn,v) ->
  let sdef = synthdef dn $ out 0 (sinOsc AR (control KR cn v) 0)
  in  case parse synthDefFile (B.concat $ BL.toChunks $ synthdefData sdef) of
    Done _ (SynthDefFile _ (def:_)) ->
      sdName def == dn &&
      any (\pp -> ppName pp == cn) (sdParamNames def) &&
      any (\us -> usName us == "SinOsc") (sdUGenSpecs def)
    _                               -> False
