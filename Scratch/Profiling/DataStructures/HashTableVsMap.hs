{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Comparing performance of HashTable and Map.

-}
module HashTableVsMap where

import Criterion.Main
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.HashTable as H

main :: IO ()
main = do
  keys <- englishWords
  m <- wordMap
  h <- wordHashTable
  defaultMain
    [ bench "map fromList" (whnf M.fromList (zip keys [1..]))
    , bench "hash table fromList" (whnf (H.fromList H.hashString) (zip keys [1..]))
    , bench "map lookup" (whnf (map $ \k -> M.lookup k m) keys)
    , bench "hash table lookup" (whnf (map $ \k -> H.lookup h k) keys)
    , bench "map delete" (whnf (map $ \k -> M.delete k m) keys)
    , bench "hash table delete" (whnf (map $ \k -> H.delete h k) keys)
    ]

englishWords :: IO [String]
englishWords = return . words =<< readFile "/usr/share/dict/words"

wordMap :: IO (M.Map String Int)
wordMap = do
  values <- englishWords
  return $ M.fromList $ zip values [1..]

wordHashTable :: IO (H.HashTable String Int)
wordHashTable = do
  values <- englishWords
  H.fromList H.hashString (zip values [1..])