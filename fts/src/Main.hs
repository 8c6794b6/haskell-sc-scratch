{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Data (Data, Typeable)

import System.Console.CmdArgs (cmdArgs, modes)

import qualified Fts.Command.Index as I
import qualified Fts.Command.Serve as S

-- | Data type for parsing command line args.
data Fts
  = Serve { port :: Int
          , dbPath :: FilePath
          , templatePath :: FilePath }
  | Index { dbPath :: FilePath
          , targetPath :: FilePath }
  deriving (Eq, Show, Data, Typeable)

-- | Parse args, run command.
main :: IO ()
main = do
    arg <- cmdArgs $ modes [server, indexer]
    case arg of
      Index d t   -> I.run d t
      Serve p d t -> S.run p d t

-- | Default setting for @fts serve@.
server :: Fts
server = Serve 8000 "db" "templates"

-- | Default setting for @fts index@.
indexer :: Fts
indexer = Index "db" "target"
