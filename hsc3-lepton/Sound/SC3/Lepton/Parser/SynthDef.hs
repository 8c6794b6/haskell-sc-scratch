{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (DeriveDataTypeable)
--
-- Parse contents of sc synth definition file.
--
-- For detail of synth definition file format, see
-- /Synth-Definition-File-Format.html/ in SuperCollider help file.
--
-- This module contains parser for synthdef spec ver.1.
--
module Sound.SC3.Lepton.Parser.SynthDef
  ( -- * Example
    -- $example

    -- * Type
    SynthDefFile(..)
  , SynthDef(..)
  , ParamPair(..)
  , UGenSpec(..)
  , InputSpec(..)

  , Result      -- from Attoparsec
  , IResult(..) -- from Attoparsec

    -- * Parser

    -- ** Running Parser
  , parseSynthDefFile
  , parse      -- from Attoparsec

    -- ** Synth Definition elements
  , synthDefFile
  , synthDefSpec
  , parameterName
  , ugenSpec
  , inputSpec
  , outputSpec

    -- * Pretty printing
  , prettyDefFile
  , prettyDef
  ) where

import Control.Applicative
import Data.Data
import Data.Int
import Text.PrettyPrint

import Data.Serialize hiding (Result(..))
import Data.Attoparsec
import Data.Attoparsec.Char8 (anyChar)

import Sound.OSC.Coding.Byte (decode_f32)
import Sound.SC3 (binaryName, unaryName)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

{- $example

Suppose that, synthdef named /default/ is written to file path shown below:

>>> sd <- parseSynthDefFile "$HOME/.local/share/SuperCollider/synthdefs/default.scsyndef"
>>> case sd of Done sd' _ -> print (prettyDefFile sd')
synthdef spec ver.1
default
  parameters:
    0: out (0.0)
    1: freq (440.0)
    2: amp (0.1)
    3: pan (0.0)
    4: gate (1.0)
  ugen specs:
    0: Control ir (out:ir)
    1: Control (1) kr (out:kr) (out:kr) (out:kr) (out:kr)
    2: VarSaw ar (ugen 1:0) (0.0) (0.3) (out:ar)
    3: Linen kr (ugen 1:3) (1.0e-2) (0.7) (0.3) (2.0) (out:kr)
    4: Rand ir (-0.4) (0.0) (out:ir)
    5: (+) kr (ugen 1:0) (ugen 4:0) (out:kr)
    6: VarSaw ar (ugen 5:0) (0.0) (0.3) (out:ar)
    7: (+) ar (ugen 2:0) (ugen 6:0) (out:ar)
    8: Rand ir (0.0) (0.4) (out:ir)
    9: (+) kr (ugen 1:0) (ugen 8:0) (out:kr)
    10: VarSaw ar (ugen 9:0) (0.0) (0.3) (out:ar)
    11: (+) ar (ugen 7:0) (ugen 10:0) (out:ar)
    12: Rand ir (4000.0) (5000.0) (out:ir)
    13: Rand ir (2500.0) (3200.0) (out:ir)
    14: XLine kr (ugen 12:0) (ugen 13:0) (1.0) (0.0) (out:kr)
    15: LPF ar (ugen 11:0) (ugen 14:0) (out:ar)
    16: (*) ar (ugen 15:0) (ugen 3:0) (out:ar)
    17: Pan2 ar (ugen 16:0) (ugen 1:2) (ugen 1:1) (out:ar) (out:ar)
    18: OffsetOut ar (ugen 0:0) (ugen 17:0) (ugen 17:1)

-}

------------------------------------------------------------------------------
--
-- Types
--
------------------------------------------------------------------------------

data SynthDefFile = SynthDefFile
  { sdVersion :: Int32
  , sdDefs :: [SynthDef] }
  deriving (Eq, Show, Data, Typeable)

data SynthDef = SynthDef
  { sdName :: String
  , sdConstants :: [Float]
  , sdParameters :: [Float]
  , sdParamNames :: [ParamPair]
  , sdUGenSpecs :: [UGenSpec]
  } deriving (Eq, Show, Data, Typeable)

data ParamPair = ParamPair
  { ppName :: String
  , ppIndex :: Int16
  } deriving (Eq, Show, Data, Typeable)

data UGenSpec = UGenSpec
  { usName :: String
  , usRate :: Int8
  , usSpecial :: Int16
  , usInputSpecs :: [InputSpec]
  , usOutputSpecs :: [Int8]
  } deriving (Eq, Show, Data, Typeable)

data InputSpec
  = IsConstant Int16
  | IsUGenOut Int16 Int16
  deriving (Eq, Show, Data, Typeable)

------------------------------------------------------------------------------
--
-- Parsers
--
------------------------------------------------------------------------------

parseSynthDefFile :: FilePath -> IO (Result SynthDefFile)
parseSynthDefFile path = parse synthDefFile <$> B.readFile path

synthDefFile :: Parser SynthDefFile
synthDefFile = do
  _ <- string $ C8.pack "SCgf"
  version <- int32
  numSynth <- int16
  sdefs <- count (fromIntegral numSynth) synthDefSpec
  return $ SynthDefFile version sdefs

synthDefSpec :: Parser SynthDef
synthDefSpec = do
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
  return $ UGenSpec className calcRate specialIndex inputSpecs outputSpecs

inputSpec :: Parser InputSpec
inputSpec = do
  isIdxOrConstant <- int16
  idx <- int16
  return $ if isIdxOrConstant == (-1)
    then IsConstant idx
    else IsUGenOut isIdxOrConstant idx

outputSpec :: Parser Int8
outputSpec = int8

pstring :: Parser String
pstring = do
  numChar <- anyChar
  str <- count (fromEnum numChar) anyChar
  return str
{-# INLINE pstring #-}

int8 :: Parser Int8
int8 = manyBytes 1
{-# INLINE int8 #-}

int16 :: Parser Int16
int16 = manyBytes 2
{-# INLINE int16 #-}

int32 :: Parser Int32
int32 = manyBytes 4
{-# INLINE int32 #-}

float32 :: Parser Float
float32 = do
  fourBytes <- count 4 anyWord8
  let w = decode_f32 . BL.fromChunks . (:[]) $ B.pack fourBytes
  return w
{-# INLINE float32 #-}

manyBytes :: Serialize a => Int -> Parser a
manyBytes n = (f . decode . B.pack) <$> (count n anyWord8)
  where
    f (Right x) = x
    f _         = error "Failed to decode"
{-# INLINE manyBytes #-}

------------------------------------------------------------------------------
--
-- Pretty printing
--
------------------------------------------------------------------------------

prettyDefFile :: SynthDefFile -> Doc
prettyDefFile (SynthDefFile ver defs) =
  text "synthdef spec ver."  <> int (fromIntegral ver) $$
  vcat (map prettyDef defs)

prettyDef :: SynthDef -> Doc
prettyDef (SynthDef n cs ps pns uss) =
  text n $$
  nest 2 (text "parameters" <> colon $$ nest 2
          (vcat (map (prettyPair ps) pns))) $$
  nest 2 (text "ugen specs" <> colon $$ nest 2
          (vcat (zipWith (\i u -> int i <> colon <+> prettyUS cs u) [0..] uss)))

prettyPair :: [Float] -> ParamPair -> Doc
prettyPair ps (ParamPair n idx) =
  int (fromIntegral idx) <> colon <+> text n <+> parens (float $ ps!!(fromIntegral idx))

prettyUS :: [Float] -> UGenSpec -> Doc
prettyUS cs (UGenSpec n r sp is os) =
  prettyUGenName n sp <+> prettyRate r <+>
  hsep (map (prettyIs cs) is) <+>
  hsep (map (\k -> parens $ text "out:" <> prettyRate k) os)

prettyUGenName :: String -> Int16 -> Doc
prettyUGenName n i
  | n == "BinaryOpUGen" = parens $ text $ binaryName $ fromIntegral i
  | n == "UnaryOpUGen"  = text $ unaryName $ fromIntegral i
  | i /= 0              = text n <+> parens (int $ fromIntegral i)
  | otherwise           = text n

prettyRate :: Int8 -> Doc
prettyRate n = case n of
  0 -> text "ir"
  1 -> text "kr"
  2 -> text "ar"
  3 -> text "dr"
  _ -> text $ "Unknown rate: " ++ show n

prettyIs :: [Float] -> InputSpec -> Doc
prettyIs cs (IsConstant idx) = parens $ float (cs !! (fromIntegral idx))
prettyIs _  (IsUGenOut ugi oi) =
  parens $ text "ugen" <+> int (fromIntegral ugi) <> colon <> int (fromIntegral oi)
