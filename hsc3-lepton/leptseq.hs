{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (DeriveDataTypeable)

Command line wrapper for pattern server.
-}
module Main where

import System.Console.CmdArgs
import Sound.SC3.Lepton.Pattern.Server3

data LeptSeq = LeptSeq
  { ls_port :: Int
  , sc_host :: String
  , sc_protocol :: Protocol
  , sc_port :: Int
  } deriving (Data, Typeable)

defaultLeptSeq :: LeptSeq
defaultLeptSeq = LeptSeq
  { ls_port = 58110
    &= typ "PORT" &= name "l"
    &= help "leptseq port (default 58110)"
  , sc_host = "127.0.0.1"
    &= typ "HOST" &= name "h"
    &= help "scsynth host to connect (default 127.0.0.1)"
  , sc_protocol = Udp
    &= typ "PROTOCOL" &= name "t"
    &= help "scsynth protocol to connect (default udp)"
  , sc_port = 57110
    &= typ "PORT" &= name "p"
    &= help "scsynth port to connect (default 57110)" }
  &= program "leptseq"
  &= summary "leptseq: pattern sequence server"

main :: IO ()
main = do
  arg <- cmdArgs defaultLeptSeq
  go (ls_port arg) (sc_host arg) (sc_protocol arg) (sc_port arg)
