{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|

Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (DeriveDataTypeable)

Module to parse the contents of synthdef files.

-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Data
import Data.Int
import Data.Word

import Data.Binary
import Data.Attoparsec
import Data.Attoparsec.Char8 (anyChar)
import System.Directory
import System.FilePath

import Sound.OpenSoundControl
import Sound.SC3 hiding (synthdef, Binary)
import Sound.SC3.Lepton

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8

data SynthDefFile = SynthDefFile
  { sdVersion :: Int32
  , sdDefs :: [SynthDef] }
  deriving (Eq, Show, Data, Typeable)

data SynthDef = SynthDef
  { sdName :: String
  , sdConstants :: [Double]
  , sdParameters :: [Double]
  , sdParamNames :: [ParamPair]
  , sdUgenSpecs :: [UGenSpec]
  } deriving (Eq, Show, Data, Typeable)

data ParamPair = ParamPair
  { ppName :: String
  , ppIndex :: Int16
  } deriving (Eq, Show, Data, Typeable)

data UGenSpec = UGenSpec
  { usName :: String
  , usRate :: Int8
  , usInputSpecs :: [InputSpec]
  , usOutputSpecs :: [Int8]
  } deriving (Eq, Show, Data, Typeable)

data InputSpec
  = IsConstant Int16
  | IsUgenOut Int16
  deriving (Eq, Show, Data, Typeable)

simple = out 0 (sinOsc ar (control kr "freq" 440) 0)
simple2 = out 0 (pan2 sig ("pan"@@0) 1) where
  sig = sinOsc ar ("freq"@@440) 0 * e
  e = envGen kr g 1 0 1 DoNothing $ envPerc 1e-12 0.5
  g = "t_trig"@@1

go defname = parseSynthdefFile $ synthDefRoot </> defname ++ ".scsyndef"
synthDefRoot = "/home/atsuro/share/SuperCollider/synthdefs/"

parseSynthdefFile :: FilePath -> IO (Result SynthDefFile)
parseSynthdefFile path =
  parse synthdefFile <$> BS.readFile path

synthdefFile :: Parser SynthDefFile
synthdefFile = do
  string $ C8S.pack "SCgf"
  version <- int32
  numSynth <- int16
  sdefs <- count (fromIntegral numSynth) synthdefSpec
  return $ SynthDefFile version sdefs

synthdefSpec :: Parser SynthDef
synthdefSpec = do
  name <- pstring
  numConstant <- int16
  constants <- count (fromIntegral numConstant) float32
  numParameters <- int16
  parameters <- count (fromIntegral numParameters) float32
  numParamNames <- int16
  paramNames <- count (fromIntegral numParamNames) parameterName
  numUGens <- int16
  ugens <- count (fromIntegral numUGens) ugenSpec
  return $ SynthDef name constants parameters paramNames ugens

parameterName :: Parser ParamPair
parameterName = ParamPair <$> pstring <*> int16

ugenSpec :: Parser UGenSpec
ugenSpec = do
  className <- pstring
  calcRate <- int8
  numInput <- int16
  numOutput <- int16
  specialIndex <- int16
  inputSpecs <- count (fromIntegral numInput) inputSpec
  outputSpecs <- count (fromIntegral numOutput) outputSpec
  return $ UGenSpec className calcRate inputSpecs outputSpecs

inputSpec :: Parser InputSpec
inputSpec = do
  isIdxOrConstant <- int16
  idx <- int16
  return $ if isIdxOrConstant == (-1)
     then IsConstant idx
     else IsUgenOut idx

outputSpec :: Parser Int8
outputSpec = int8

pstring :: Parser String
pstring = do
  numChar <- anyChar
  str <- count (fromEnum numChar) anyChar
  return str

int8 :: Parser Int8
int8 = manyBytes 1

int16 :: Parser Int16
int16 = manyBytes 2

int32 :: Parser Int32
int32 = manyBytes 4

float32 :: Parser Double
float32 = do
  fourBytes <- count 4 anyChar
  let w = decode_f32 $ C8.pack fourBytes :: Double
  return w

manyBytes :: Binary a => Int -> Parser a
manyBytes n = (decode . B.pack) <$> (count n anyWord8)
{-# INLINE manyBytes #-}

test :: IO ()
test = do
  names <- getDirectoryContents synthDefRoot
  forM_ (filter (`notElem` [".", ".."]) names) $ \n ->
    print =<< (parseSynthdefFile $ synthDefRoot </> n)

main :: IO ()
main = test