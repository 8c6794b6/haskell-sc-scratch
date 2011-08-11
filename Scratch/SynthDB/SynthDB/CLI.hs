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
  | Serve { port :: Int }
  deriving (Eq,Show,Data,Typeable)

main :: IO ()
main = do
  arg <- cmdArgs $ modes
    [ Init &=
      help "initialize database"
    , Search {qs = def &= args &= typ "UGEN NAMES"} &=
      help "search synthdef by ugen names"
    , Dump {keyName = def &= args &= typ "SYNTHDEF NAME"} &=
      help "dump contents of specified synthdef"
    , Serve {port = 8000 &= help "PORT NUMBER"} &=
      help "start server for web ui" ]
  case arg of
    Init     -> initDB
    Search q -> search q
    Dump n   -> dump n
    Serve p  -> serve p
