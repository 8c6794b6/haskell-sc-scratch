{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Comparing performance of data structures that supports mapping.
Map, HashTable, HashMap in unordered-containers.

-}
module Mappings where

import Control.Exception hiding (catch)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (chr,ord)
import Data.Int (Int32)

import Criterion.Main

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as C8
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable as HT
import qualified Database.KyotoCabinet.Db as KC

main :: IO ()
main = do
  keys <- englishWords
  m <- wordMap
  hm <- wordHashMap
  ht <- wordHashTable
  kc <- wordKcDb
  defaultMain
    [ bench "Map fromList" (whnf M.fromList (zip keys [1..]))
    , bench "HashMap fromList" (whnf HM.fromList (zip keys [1..]))
    , bench "HashTable fromList" --- XXX: Are elements inserted?
      (whnf (HT.fromList hashByteString) (zip keys [1..]))

    , bench "Map lookup" (whnf (map $ \k -> M.lookup k m) keys)
    , bench "HashMap lookup" (whnf (map $ \k -> HM.lookup k hm) keys)
    , bench "HashTable lookup" (whnf (map $ \k -> HT.lookup ht k) keys)
    , bench "KcDb lookup" (whnf (map $ \k -> KC.kcdbget kc) keys)

    , bench "Map delete" (whnf (map $ \k -> M.delete k m) keys)
    , bench "HashMap delete" (whnf (map $ \k -> HM.delete k hm) keys)
    , bench "HashTable delete" (whnf (map $ \k -> HT.delete ht k) keys)
    , bench "KcDb delete" (whnf (map $ \k -> KC.kcdbremove kc k) keys)
    ]
  KC.kcdbclose kc

englishWords :: IO [ByteString]
englishWords = return . C8.words =<< C8.readFile "/usr/share/dict/words"

wordMap :: IO (M.Map ByteString Int)
wordMap = do
  values <- englishWords
  return $ M.fromList $ zip values [1..]

wordHashMap :: IO (HM.HashMap ByteString Int)
wordHashMap = do
  values <- englishWords
  return $ HM.fromList $ zip values [1..]

wordHashTable :: IO (HT.HashTable ByteString Int)
wordHashTable = do
  values <- englishWords
  HT.fromList hashByteString (zip values [1..])

hashByteString :: C8.ByteString -> Int32
hashByteString = C8.foldl' f golden where
  f m c = fromIntegral (C8.c2w c) * magic + hashInt32 m
  golden = round ((sqrt 5 - 1) * 2 ^ 32)
  magic = 0xdeadbeef
  hashInt32 = id -- may not good as HT.hashInt

-- | Returned db is kept opened.
wordKcDb :: IO KC.KcDb
wordKcDb = do
  values <- englishWords
  db <- KC.kcdbnew
  KC.kcdbopen db dbPath [] [KC.KCOWRITER, KC.KCOCREATE]
  zipWithM_ (\k v -> KC.kcdbset db k (C8.pack $ show v)) values [1..]
  return db

dbPath :: String
dbPath = "*"
-- dbPath = "%"
-- dbPath = "foo.kch" -- lookup: 29.34853,  delete: 29.43341
-- dbPath = "foo.kct" -- slower than .kch

kcdbLookup :: ByteString -> IO (Maybe ByteString)
kcdbLookup key = KC.kcwithdbopen dbPath [] [KC.KCOREADER] $ \db -> do
  KC.kcdbget db key
