module Main where

import Sound.SC3 (withSC3)

import Pssd.TelephoneBells.OldStyle

main :: IO ()
main = withSC3 ringBell