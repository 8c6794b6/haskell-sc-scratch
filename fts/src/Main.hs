{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Data (Data, Typeable)

import System.Console.CmdArgs
  ( (&=)
  , cmdArgs
  , help
  , modes
  , summary
  , typ
  , typDir )
import qualified Fts.Command.Index as I
import qualified Fts.Command.Serve as S

-- | Data type for parsing command line args.
data Fts
  = Serve { port :: Int
          , db :: FilePath
          , template :: FilePath }
  | Index { db :: FilePath
          , target :: FilePath }
  deriving (Eq, Show, Data, Typeable)

-- | Parse args, run command.
main :: IO ()
main = do
    arg <- cmdArgs $ modes [server, indexer]
           &= summary "fts: simple full text search with tokyo dystopia"
    case arg of
      Index d t   -> I.run d t
      Serve p d t -> S.run p d t

-- | Default setting for @fts serve@.
server :: Fts
server = Serve
  { port = 8000 &= help "port number to serve" &= typ "NUM"
  , db = "db" &= help "path to indexed database" &= typDir
  , template = "templates" &= help "path to template directory" &= typDir }

-- | Default setting for @fts index@.
indexer :: Fts
indexer = Index
  { db = "db" &= help "output path for indexed data" &= typDir
  , target = "target" &= help "path for target html files" &= typDir }
