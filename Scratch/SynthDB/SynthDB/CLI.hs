{-# LANGUAGE DeriveDataTypeable #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

-}
module SynthDB.CLI where

import System.Console.CmdArgs

import SynthDB.Persist
import SynthDB.Web

data SynthDB
  = Init
  | Search { qs :: String }
  | Dump { keyName :: String }
  | Serve
  deriving (Eq,Show,Data,Typeable)

main :: IO ()
main = do
  arg <- cmdArgs $ modes
    [ Init &=
      help "initialize database"
    , Search {qs = def &= args &= typ "ugen names"} &=
      help "search synthdef by ugen names"
    , Dump {keyName = def &= args &= typ "synthdef name"} &=
      help "dump contents of specified synthdef"
    , Serve &=
      help "start server for web ui"]
  case arg of
    Init     -> initDB
    Search q -> search q
    Dump n   -> dump n
    Serve    -> serve
