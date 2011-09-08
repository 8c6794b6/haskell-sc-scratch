{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (DeriveDataTypeable)

Command line wrapper for pattern sequencer.

-}
module Main where

import Control.Exception (bracket)
import System.Console.CmdArgs (cmdArgs)

import Sound.SC3.Lepton.Pattern.Server

main :: IO ()
main = bracket setup release runServer where
  setup = mkInitEnv =<< cmdArgs (LeptSeq 58110 ("127.0.0.1",57110,Udp))
  release = shutdownServer
