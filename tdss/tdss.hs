{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Command line interface for building indexed database and serving the site.
--
-- To show help:
--
-- > $ tdss --help
--
-- Build the indexed database for the site.
--
-- > $ tdss index -t target -d db
--
-- Serve the site.
--
-- > $ tdss serve -p 8000 -d db -t templates
--
module Main (main) where

import Data.Data (Data, Typeable)

import System.Console.CmdArgs 
  ((&=), cmdArgs, help, modes, summary, typ, typDir)
import qualified Tdss.Command.Index as I
import qualified Tdss.Command.Serve as S

-- | Data type for parsing command line args.
data Tdss
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
           &= summary "tdss: simple full text search with tokyo dystopia"
    case arg of
      Index d t   -> I.run d t
      Serve p d t -> S.run p d t

-- | Default setting for @tdss serve@.
server :: Tdss
server = Serve
  { port = 8000 &= help "port number to serve" &= typ "NUM"
  , db = "db" &= help "path to indexed database" &= typDir
  , template = "templates" &= help "path to template directory" &= typDir }

-- | Default setting for @tdss index@.
indexer :: Tdss
indexer = Index
  { db = "db" &= help "output path for indexed data" &= typDir
  , target = "target" &= help "path for target html files" &= typDir }
