{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Main where

import System.Console.CmdArgs
import Sound.OpenSoundControl
import Sound.SC3.Lepton.CLI.SCShell

main :: IO ()
main = do
  info <- cmdArgs defaultSCShell
  putStrLn $ "connecting to " ++ host info ++ ":" ++ show (port info)
    ++ ", " ++ show (protocol info)
  let con = case protocol info of
        UDP -> Right `fmap` openUDP (host info) (port info)
        TCP -> Left `fmap` openTCP (host info) (port info)
  withTransport con (scShell (host info) (port info))

data SCShell = SCShell
  { protocol :: Protocol
  , host :: String
  , port :: Int
  } deriving (Eq, Show, Data, Typeable)

data Protocol = TCP | UDP deriving (Eq, Show, Data, Typeable)

defaultSCShell :: SCShell
defaultSCShell = SCShell {protocol=UDP, host="127.0.0.1", port=57110}
