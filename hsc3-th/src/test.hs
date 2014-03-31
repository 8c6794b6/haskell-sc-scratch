{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown
-}
module Main where

import Sound.SC3
import Sound.SC3.TH.Synthdef (synthdefGenerator)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit

synth_foo :: UGen
synth_foo = out 0 (sinOsc AR 440 0 * 0.1)

synth_bar :: UGen
synth_bar = out 0 (saw AR 440 * 0.1)

buzz_is_not_a_synthdef :: UGen
buzz_is_not_a_synthdef = lfCub KR 0 0

synthdefs :: [Synthdef]
synthdefs = $(synthdefGenerator)

main :: IO ()
main = defaultMain $ testGroup "Unit Tests"
    [ testCase "length synthdefs == 2" $
        length synthdefs @?= 2
    , testCase "\"foo\" == synth_foo" $
        let foo = head $ filter ((== "foo") . synthdefName) synthdefs
        in  foo @?= synthdef "foo" synth_foo
    , testCase "\"bar\" == synth_bar" $
        let bar = head $ filter ((== "bar") . synthdefName) synthdefs
        in  bar @?= synthdef "bar" synth_bar
    ]
