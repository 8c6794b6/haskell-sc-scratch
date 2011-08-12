------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Peek inside synthdef file to get control parameter names.
--
module Sound.SC3.Lepton.CLI.HscShell.SynthdefPeeker where

import Data.Int
import System.Directory

import Sound.SC3.Lepton.Parser

-- | Get param names from synthdef file path.
getParamNames :: FilePath -> IO [String]
getParamNames = parseParamNames

parseParamNames :: FilePath -> IO [String]
parseParamNames defFile = do
  exists <- doesFileExist defFile
  if exists
    then do res <- parseSynthDefFile defFile
            return $ case res of
              Done _ sd -> map ppName . concatMap sdParamNames $ sdDefs sd
              _         -> []
    else return []
