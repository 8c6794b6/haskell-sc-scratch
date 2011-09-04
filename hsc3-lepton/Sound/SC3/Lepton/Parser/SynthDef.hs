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
module Sound.SC3.Lepton.Parser.SynthDef
  ( -- * Type
    SynthDefFile(..)
  , SynthDef(..)
  , ParamPair(..)
  , UGenSpec(..)
  , InputSpec(..)
  , Result(..) -- from Attoparsec

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
    -- ** Primitives
  , pstring
  , int8
  , int16
  , int32
  , float32

    -- * Pretty printing
  , prettyDefFile
  , prettyDef
  ) where

import Control.Applicative
import Data.Data
import Data.Int
import Text.PrettyPrint

import Data.Serialize hiding (Result)
import Data.Attoparsec
import Data.Attoparsec.Char8 (anyChar)

import Sound.OpenSoundControl.Coding.Byte (decode_f32)
import Sound.SC3 (binaryName, unaryName)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

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
  , sdConstants :: [Double]
  , sdParameters :: [Double]
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
  string $ C8.pack "SCgf"
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

float32 :: Parser Double
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

prettyPair :: [Double] -> ParamPair -> Doc
prettyPair ps (ParamPair n idx) =
  int (fromIntegral idx) <> colon <+> text n <+> parens (double $ ps!!(fromIntegral idx))

prettyUS :: [Double] -> UGenSpec -> Doc
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

prettyIs :: [Double] -> InputSpec -> Doc
prettyIs cs (IsConstant idx) = parens $ double (cs !! (fromIntegral idx))
prettyIs _  (IsUGenOut ugi oi) =
  parens $ text "ugen" <+> int (fromIntegral ugi) <> colon <> int (fromIntegral oi)
