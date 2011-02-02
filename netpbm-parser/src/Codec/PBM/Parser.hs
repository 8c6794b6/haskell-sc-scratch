------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- Description : Module for parsing PGM file
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
--
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Module for PGM parser.
--
-- Examples:
-- 
-- Parse raw PGM file.
-- 
-- > > raw <- L.readFile "images/raw.pgm"
-- > > parse parsePGM raw
-- > Right Greymap 200x200 255
-- 
-- Parse plain PGM file.
-- 
-- > > plain <- L.readFile "images/plain.pgm"
-- > > parse parsePGM plain
-- > Right Greymap 200x200 255
--
-- Parse raw PPM file.
-- 
-- > > ppm <- L.readFile "images/raw.ppm"
-- > > parse parseRawPPM ppm
-- > > ....
-- 
module Codec.PBM.Parser where

import Control.Applicative ((<$>))
import Data.Array (Array, listArray)
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Codec.PBM.Types (Greymap(..), ParseState(..))

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
  }
                  
instance Functor Parse where
  fmap f p = p >>= \r -> return (f r)
  
instance Monad Parse where
  return a = Parse (\s -> Right (a, s))
  m >>= k = Parse $ \s -> case runParse m s of
    Right (res, s') -> runParse (k res) s'
    Left e          -> Left e
  fail err = Parse $ \s ->
    Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
  initState { offset = newOffset }

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
  case runParse parser (ParseState initState 0) of
    Left err          -> Left err
    Right (result, _) -> Right result

-- | Identify header and parse plain or raw PGM file.
parsePGM :: Parse Greymap
parsePGM =
  parseWhileWith w2c notWhite >>= \header -> skipSpaces >>
  parseNat >>= \width -> skipSpaces >>
  parseNat >>= \height -> skipSpaces >>
  parseNat >>= \maxGrey ->
  parseByte >>
  let fn = case header of
        "P2" -> parseChars
        "P5" -> if maxGrey < 256 then parseBytes else parse2Bytes
        _    -> const $ fail "invalid header"
  in  fn (width * height) >>= \bitmap ->
      return (Greymap width height maxGrey bitmap)

parsePPM :: Parse Pixmap
parsePPM = 
  parseWhileWith w2c notWhite >>= \header -> skipSpaces >>
  assert (header == "P6") "invalid raw header" >>
  parseNat >>= \width -> skipSpaces >>
  parseNat >>= \height -> skipSpaces >>
  parseNat >>= \maxValue ->
  assert (maxValue == 255) "max value out of spec" >>
  parseByte >>
  parseTimes (width * height) parseRGB >>= \pxs ->
  return (listArray ((0,0),(height-1,width-1)) pxs)
  
type Pixmap = Array (Int,Int) RGB
type RGB = (Word8,Word8,Word8)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = return []
parseTimes n p = p >>= \x -> (x:) <$> parseTimes (n-1) p

parseRGB :: Parse RGB
parseRGB =
  parseByte >>= \r ->
  parseByte >>= \g ->
  parseByte >>= \b ->
  return (r,g,b)

-- | Partial solution to exercise 2.
-- 
-- > In our description of 'raw' PGM files, we omitted a small detail. If
-- > the 'maximum grey' value in the header is less than 256, each pixel is
-- > represented by a single byte. However, it can range up to 65535, in
-- > which case each pixel will be represented by two bytes, in big endian
-- > order (most significant byte first). Rewrite the raw PGM parser to
-- > accommodate both the single- and double-byte pixel
-- 
-- This function takes 2 function used for parsing the body contents of pgm 
-- file.
--
parsePGMWith :: String                      -- ^ Header magic string
             -> (Int -> Parse L.ByteString) -- ^ Parser for maxGrey < 256
             -> (Int -> Parse L.ByteString) -- ^ parser for maxGrey >= 256
             -> Parse Greymap
parsePGMWith mg fn1 fn2  =
  parseWhileWith w2c notWhite >>= \header -> skipSpaces >>
  assert (header == mg) "invalid header" >>
  parseNat >>= \width -> skipSpaces >>
  parseNat >>= \height -> skipSpaces >>
  parseNat >>= \maxGrey ->
  parseByte >>
  let fn = if maxGrey < 256 then fn1 else fn2
  in  fn (width * height) >>= \bitmap ->
      return (Greymap width height maxGrey bitmap)

parseRawPGM :: Parse Greymap
parseRawPGM = parsePGMWith "P5" parseBytes parse2Bytes

-- | Parse each 1 bytes for specified number
parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState >>= \st ->
  let n' = fromIntegral n
      (h, t) = L.splitAt n' (string st)
      st' = st { offset = offset st + L.length h, string = t }
  in  putState st' >>
      assert (L.length h == n') "end of input" >>
      return h

-- | Parse each 2 bytes for specified number.
parse2Bytes :: Int -> Parse L.ByteString
parse2Bytes n =
  getState >>= \st ->
  let n' = fromIntegral n * 2
      (h, t) = L.splitAt n' (string st)
      numPixels = L.length h `div` 2
      st' = st { offset = offset st + numPixels, string = t }
  in  putState st' >>
      assert (L.length h == n') "end of input" >>
      return h
      
parseNBytes :: Int -> Int -> Parse L.ByteString      
parseNBytes n m = do
  st <- getState
  let m' = fromIntegral $ m * n
      (h,t) = L.splitAt m' (string st)
      numPixels = L.length h `div` (fromIntegral n)
      st' = st { offset = offset st + numPixels, string = t }
  putState st'
  assert (L.length h == m') "end of input"
  return h

-- | This is the solution for exercise 1 in end of chapter 10.
--
-- > Write a parser for 'plain' PGM files.  A plain PGM file has
-- > 'P2' header, and values are represented with ASCII decimal numbers.
--
-- Parser for 'plain' PGM files.
--
parsePlainPGM :: Parse Greymap
parsePlainPGM = parsePGMWith "P2" parseChars parseChars

parseChars :: Int -> Parse L.ByteString
parseChars n =
  getState >>= \st ->
  let string' = L8.concat $ take n $ L8.split ' ' (string st)
      offset' = fromIntegral n + offset st
  in  putState (ParseState string' offset') >>
      return string'

parseByte :: Parse Word8
parseByte =
  getState >>= \initState -> case L.uncons (string initState) of
    Nothing                ->
      fail "no more input"
    Just (byte, remainder) ->
      putState newState >>= const (return byte)
      where
        newState = initState { string = remainder
                             , offset = newOffset }
        newOffset = offset initState + 1

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) >>= \mp ->
  if mp == Just True
     then parseByte >>= \b -> (b:) <$> parseWhile p
     else return []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace >> return ()

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit >>= \digits ->
  if null digits
     then fail "no more input"
     else let n = read digits
          in  if n < 0
                 then fail "integer overflow"
                 else return n

notWhite :: Char -> Bool
notWhite = (`notElem` "\r\n\t")

w2c :: Word8 -> Char
w2c = chr . fromIntegral

assert :: Bool -> String -> Parse ()
assert p msg | p         = return ()
             | otherwise = fail msg


