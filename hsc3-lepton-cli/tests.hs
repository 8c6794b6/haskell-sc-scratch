{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Exit

import Test.Sound.SC3.Lepton.CLI.Common
import qualified Test.Sound.SC3.Lepton.CLI.Parser as Parser
import qualified Test.Sound.SC3.Lepton.CLI.SCShellCmd as SCShellCmd
import qualified Test.Sound.SC3.Lepton.CLI.SCZipper as SCZipper

main :: IO ()
main = do
  parser <- Parser.runTests
  sczipper <- SCZipper.runTests
  scshellcmd <- SCShellCmd.runTests
  if any (not . isSuccess) (parser ++ sczipper ++ scshellcmd)
    then exitFailure
    else exitSuccess
