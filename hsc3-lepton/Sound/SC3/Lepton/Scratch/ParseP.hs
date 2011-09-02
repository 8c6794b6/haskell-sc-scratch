{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (OverloadedStrings)

Parsing patterns encoded in ByteString.
-}
module Sound.SC3.Lepton.Scratch.ParseP where

{-

TODO:

* Handle functions defined in classes S instanciates. (Floating, UnaryOp, etc)

* Run server to play patterns, receive via UDP, enable pausing, running new
  patterns with specifying offset time.

-}

import Control.Applicative
import Data.ByteString (ByteString)
import Prelude hiding (takeWhile)
import System.Random

import Data.Attoparsec hiding (takeWhile)
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Sound.SC3 hiding ((<*), osc)

import Sound.SC3.Lepton (R(..), S(..), ToOSC(..), MsgType(..))
import Sound.SC3.Lepton.Scratch.RespTest (pspe,gospe'p, msg01, loop03)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M

import qualified Sound.SC3.Lepton as P

runPatternFile :: FilePath -> IO ()
runPatternFile path = runPattern =<< C8.readFile path

runPattern :: ByteString -> IO ()
runPattern bs = case parsePattern bs of
  Done _ r      -> audition r
  Fail _ ctxs e -> mapM_ putStrLn ctxs >> putStrLn e
  _             -> putStrLn "Partial"

parsePattern :: ByteString -> Result (R (ToOSC Double))
parsePattern = parse rPatterns

runnables :: Parser (R (ToOSC Double))
runnables = rPatterns

rPatterns = choice
  [ psnew, pnset, pmerge, ppar
  , pconcat rPatterns, pappend rPatterns, pseq intP rPatterns
  , preplicate intP rPatterns, pcycle rPatterns
  , {- prepeat rPatterns, -} pforever rPatterns
  , prandom, prange rPatterns, pchoose intP rPatterns
  , prand intP rPatterns, pshuffle rPatterns
  ]

patterns p = choice
  [ pval p, plist p, pempty
  , pconcat (patterns p), pappend (patterns p), pseq intP (patterns p)
  , preplicate intP (patterns p), pcycle (patterns p)
  , prepeat p, pforever (patterns p)
  , prandom, prange (patterns p), pchoose intP (patterns p)
  , prand intP (patterns p), pshuffle (patterns p)
  ]

{-

-- Parse and print midi note pattern from streams-patterns-event.
let Done _ r = parse (patterns number) (C8.pack $ P.showP pspe)
in  P.mapPIO_ print (r :: R Number)

-- Parse psnew and pnset.
let Done _ r = parse rPatterns (C8.pack $ P.showP (P.ppar [msg01,loop03]))
in  P.mapPIO_ print (r :: R (ToOSC Double))

-- Read pattern from file and run it.
runPatternFile "./Sound/SC3/Lepton/Scratch/pat01.txt"

-}

-- ---------------------------------------------------------------------------
-- Primitives

pempty = string "pempty" *> return P.pempty

pval p = mkP "pval" P.pval p

plist p = mkP "plist" P.plist (listOf p)

prepeat p = mkP "prepeat" P.prepeat p

-- ---------------------------------------------------------------------------
-- Looping patterns

pconcat p = mkP "pconcat" P.pconcat (listOf p)

pappend p = mkP2 "pappend" P.pappend (braced p) (braced p)

pseq p1 p2  = mkP2 "pseq" P.pseq p1 (listOf p2)

preplicate p1 p2 = mkP2 "preplicate" P.preplicate p1 (braced p2)

pcycle p = mkP "pcycle" P.pcycle (listOf p)

pforever p = mkP "pforever" P.pforever (braced p)

-- ---------------------------------------------------------------------------
-- Random patterns

prandom = string "prandom" *> return P.prandom

prange p = mkP2 "prange" P.prange (braced p) (braced p)

pchoose p1 p2 = mkP2 "pchoose" P.pchoose p1 (listOf p2)

prand p1 p2 = mkP2 "prand" P.prand p1 (listOf p2)

pshuffle p = mkP "pshuffle" P.pshuffle (listOf p)

-- ---------------------------------------------------------------------------
-- Parallel patterns

pmerge = mkP2 "pmerge" P.pmerge (braced rPatterns) (braced rPatterns)

ppar = mkP "ppar" P.ppar (listOf rPatterns)

-- ---------------------------------------------------------------------------
-- OSC message patterns

psnew = do
  string "Snew"
  name' <- skipSpace *> name
  nodeId' <- skipSpace *> nodeId
  addAction' <- skipSpace *> addAction
  targetId' <- skipSpace *> targetId
  paramList'  <- skipSpace *> paramList
  return $ P.psnew name' nodeId' addAction' targetId' paramList'

pnset = mkP2 "Nset" P.pnset targetId paramList

paramList = listOf (braced pair) where
  pair = (,) <$> name <*> (char ',' *> (fmap n2double <$> patterns number))

nodeId :: Parser (Maybe Int)
nodeId =
  (string "Nothing" *> pure Nothing) <|>
  (braced (string "Just " *> (Just . n2int <$> (number <|> braced number))))

targetId :: Parser Int
targetId = n2int <$> number

name :: Parser String
name = char '"' *> (C8.unpack <$> takeWhile (/= '"')) <* char '"'

addAction :: Parser AddAction
addAction =
  (string "AddToHead" *> pure AddToHead) <|>
  (string "AddToTail" *> pure AddToTail) <|>
  (string "AddBefore" *> pure AddBefore) <|>
  (string "AddAfter" *> pure AddAfter) <|>
  (string "AddReplace" *> pure AddReplace)

-- ---------------------------------------------------------------------------
-- Utils

intP = fmap n2int <$> braced (patterns number)

mkP n f p1 = string n *> skipSpace *> (f <$> p1)

mkP2 n f p1 p2 = f <$ string n <*> (skipSpace *> p1) <*> (skipSpace *> p2)

braced :: Parser a -> Parser a
braced a = char '(' *> skipSpace *> a <* skipSpace <* char ')'

listOf :: Parser a -> Parser [a]
listOf p = char '[' *> p `sepBy` char ',' <*  char ']'

n2int :: Number -> Int
n2int n = case n of
  I i  -> fromIntegral i
  D d  -> truncate d

n2double :: Number -> Double
n2double n = case n of
  I i -> fromIntegral i
  D d -> d

instance Random Number where
  random g = let (n,g') = random g in (D n, g')
  randomR ((I lo),(I hi)) g = let (n,g') = randomR (lo, hi) g in (I n,g')
  randomR ((D lo),(D hi)) g = let (n,g') = randomR (lo, hi) g in (D n,g')
  randomR _ _ = error "Number with mismatched constructor in randomR"

instance Random a => Random (ToOSC a) where
  random  = undefined
  randomR = undefined