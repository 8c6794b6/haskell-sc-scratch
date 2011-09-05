{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Comparing performance of Trie and Map.

-}
module TrieVsMap where

import Criterion.Main
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.Trie as T
import qualified Data.Trie.Internal as T

main :: IO ()
main = do
  keys <- englishWords
  m <- wordMap
  t <- wordTrie
  defaultMain
    [ bench "map fromList" (whnf M.fromList (zip keys [1..]))
    , bench "trie fromList" (whnf T.fromList (zip keys [1..]))
    , bench "map lookup" (whnf (map $ \k -> M.lookup k m) keys)
    , bench "trie lookup" (whnf (map $ \k -> T.lookup k t) keys)
    , bench "map delete" (whnf (map $ \k -> M.delete k m) keys)
    , bench "trie delete" (whnf (map $ \k -> T.delete k t) keys)
    ]

englishWords :: IO [C8.ByteString]
englishWords = return . C8.words =<< C8.readFile "/usr/share/dict/words"

wordMap :: IO (M.Map C8.ByteString Int)
wordMap = do
  values <- englishWords
  return $ M.fromList $ zip values [1..]

wordTrie :: IO (T.Trie Int)
wordTrie = do
  values <- englishWords
  return $ T.fromList (zip values [1..])