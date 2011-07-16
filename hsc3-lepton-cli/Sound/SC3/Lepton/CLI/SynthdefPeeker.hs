------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Peek inside synthdef file.
--
module Sound.SC3.Lepton.CLI.SynthdefPeeker where

import Control.Monad
import Data.Int
import Text.Parsec
import System.Directory
import System.FilePath

import Data.Binary
import Sound.SC3 hiding (synthdef, Binary)

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Text.Parsec.ByteString as PB

-- | Get param names from synthdef file path.
getParamNames :: FilePath -> IO [String]
getParamNames = fmap (either (const []) (concat)) . parseParamNames

parseParamNames :: String -> IO (Either ParseError [[String]])
parseParamNames defFile = do
  exists <- doesFileExist defFile
  if exists
    then PB.parseFromFile paramFromFile defFile
    else return $ Right []

paramFromFile :: PB.Parser [[String]]
paramFromFile = do
  skipBytes 8
  numSynth <- int16
  pNames <- replicateM (fromIntegral numSynth) paramNameOnly
  return pNames

skipBytes :: Int -> PB.Parser ()
skipBytes n = replicateM n anyChar >> return ()

paramNameOnly :: PB.Parser [String]
paramNameOnly = do
  synthdefName <- pstring
  numConstant <- int16
  skipBytes (fromIntegral numConstant * 4)
  numParameters <- int16
  skipBytes (fromIntegral numParameters * 4)
  numParamNames <- int16
  paramNames <- replicateM (fromIntegral numParamNames) theParamName
  return paramNames

theParamName :: PB.Parser String
theParamName = do
  name <- pstring
  skipBytes 2
  return name

int16 :: PB.Parser Int16
int16 = manyBytes 2

pstring :: PB.Parser String
pstring = do
  oneByte <- anyChar
  str <- replicateM (fromEnum oneByte) anyChar
  return str

manyBytes :: Binary a => Int -> PB.Parser a
manyBytes n = (decode . C8.pack) `fmap` (replicateM n anyChar)
