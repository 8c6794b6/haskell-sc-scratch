{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (DeriveDataTypeable)

Command line wrapper for pattern sequencer.

-}
module Main where

import Control.Exception (bracket)
import System.Console.CmdArgs

import Sound.SC3.Lepton.Pattern.Server

main :: IO ()
main = bracket setup release runServer where
  setup = do
    as <- cmdArgs defaultLeptSeq
    putStrLn $ "Starting server with port: " ++ show (port as)
    let (h,p,ptc) = sc as
    putStrLn $ concat
      ["Connecting to scsynth: ", h, ":", show p, " (", show ptc, ")"]
    mkInitEnv as
  release = shutdownServer

defaultArgs :: LeptSeq
defaultArgs = LeptSeq
  { port = 58110 &= typ "PORT" &=
           help "port used by leptseq to receive message [default:58110]"
  , sc = ("127.0.0.1",57110,Udp) &=
         typ "HOST,PORT,Protocol" &=
         help ("Host, port, and protocol of scsynth server to communicate " ++
               "[default:127.0.0.1,57110,udp]")
  }