{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad
import Data.Data
import Data.Int
import Data.Word

import Data.Binary
import Text.Parsec
import System.Directory
import System.FilePath

import Sound.OpenSoundControl
import Sound.SC3 hiding (synthdef, Binary)
import Sound.SC3.Lepton

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Text.Parsec.ByteString as PB

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

parseSynthdefFile :: FilePath -> IO (Either ParseError SynthDefFile)
parseSynthdefFile = PB.parseFromFile synthdefFile

synthdefFile :: PB.Parser SynthDefFile
synthdefFile = do
  string "SCgf"
  version <- int32
  numSynth <- int16
  sdefs <- replicateM (fromIntegral numSynth) synthdef
  return $ SynthDefFile version sdefs

synthdef :: PB.Parser SynthDef
synthdef = do
  name <- pstring
  numConstant <- int16
  constants <- replicateM (fromIntegral numConstant) float32
  numParameters <- int16
  parameters <- replicateM (fromIntegral numParameters) float32
  numParamNames <- int16
  paramNames <- replicateM (fromIntegral numParamNames) parameterName
  numUGens <- int16
  ugens <- replicateM (fromIntegral numUGens) ugenSpec
  return $ SynthDef name constants parameters paramNames ugens

parameterName :: PB.Parser ParamPair
parameterName = do
  name <- pstring
  idx <- int16
  return $ ParamPair name idx

ugenSpec :: PB.Parser UGenSpec
ugenSpec = do
  className <- pstring
  calcRate <- int8
  numInput <- int16
  numOutput <- int16
  specialIndex <- int16
  inputSpecs <- replicateM (fromIntegral numInput) inputSpec
  outputSpecs <- replicateM (fromIntegral numOutput) outputSpec
  return $ UGenSpec className calcRate inputSpecs outputSpecs

inputSpec :: PB.Parser InputSpec
inputSpec = do
  isIdxOrConstant <- int16
  idx <- int16
  return $ if isIdxOrConstant == (-1)
     then IsConstant idx
     else IsUgenOut idx

outputSpec :: PB.Parser Int8
outputSpec = do
  calcRate <- int8
  return calcRate

pstring :: PB.Parser String
pstring = do
  oneByte <- anyChar
  str <- replicateM (fromEnum oneByte) anyChar
  return str

int8 :: PB.Parser Int8
int8 = manyBytes 1

int16 :: PB.Parser Int16
int16 = manyBytes 2

int32 :: PB.Parser Int32
int32 = manyBytes 4

float32 :: PB.Parser Double
float32 = do
  fourBytes <- replicateM 4 anyChar
  let w = decode_f32 $ C8.pack fourBytes :: Double
  return w

manyBytes :: Binary a => Int -> PB.Parser a
manyBytes n = (decode . C8.pack) `fmap` (replicateM n anyChar)