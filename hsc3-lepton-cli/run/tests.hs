{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Exit

import Test.Sound.SC3.Lepton.CLI.HscShell.Common
import qualified Test.Sound.SC3.Lepton.CLI.HscShell.Parser as Parser
import qualified Test.Sound.SC3.Lepton.CLI.HscShell.Cmd as Cmd

main :: IO ()
main = do
  parser <- Parser.runTests
  scshellcmd <- Cmd.runTests
  if any (not . isSuccess) (parser ++ scshellcmd)
    then exitFailure
    else exitSuccess
