{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (DeriveDataTypeable)

Simple command line interface for synthdef database.

-}

module Kyoto.CLI where

import Control.Monad (when)
import System.Console.CmdArgs
import Kyoto.Scratch

-- | Type for command line argument parsing
data Hsckcdb
  = -- | Initialize synthdef database
    InitDB { db :: FilePath }
    -- | Search from synthdef database
  | Search  { db :: FilePath
            , query :: String }
    -- | Dump contents
  | Dump   { db :: FilePath
           , key :: String }
    -- | Show statistic information
  | Stat   { db :: FilePath
           , pStat :: Bool
           , uStat :: Bool }
  deriving (Eq,Show,Data,Typeable)

main :: IO ()
main = do
  let complain = putStrLn "No path given, try --help to see usage."
  arg <- cmdArgs $ modes
    [InitDB {db = def &= help "Path to synthdef databse" &= typ "DBPATH"}
    ,Search {db = def &= help "Path to synthdef databse" &= typ "DBPATH"
            ,query = def &= help "query to search" &= typ "QUERY"}
    ,Dump   {db = def &= help "Path to synthdef databse" &= typ "DBPATH"
            ,key = def &= help "key to dump in database" &= typ "KEY"}
    ,Stat   {db = def &= help "Path to synthdef databse" &= typ "DBPATH"
            ,pStat = def &= help "show parameter stats"
            ,uStat = def &= help "show ugen stats"}]
  case db arg of
    [] -> complain
    _  -> case arg of
      InitDB path -> initDB path
      Search path q -> mapM_ print =<< search q path
      Dump path k
        | null k    -> dumpAllSynthDefs path
        | otherwise -> dumpSynthDef path k
      Stat path p u -> do
        when p $ print . prettyParamStat =<< paramStat path
        when u $ print . prettyUR =<< ugenStat path
