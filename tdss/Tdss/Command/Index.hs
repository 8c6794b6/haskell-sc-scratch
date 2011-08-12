------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
module Tdss.Command.Index where

import Control.Monad (join)
import Control.Concurrent (newMVar, putMVar, takeMVar)
import Data.Int (Int64)
import Data.List (isSuffixOf)
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Char8 as C8

import System.Directory.Tree (readDirectoryWith)
import Text.HTML.TagSoup (Tag(..), (~==), innerText, parseTags, sections)
import qualified Database.TokyoCabinet.HDB as HDB
import qualified Database.TokyoDystopia as TD
import qualified Database.TokyoDystopia.IDB as IDB

import qualified Tdss.Model as M

-- | Index html files under specified directory and make it searchable.
run :: FilePath -- ^ DB path
    -> FilePath -- ^ Target path
    -> IO ()
run dbPath targetPath = do
  putStrLn "Start indexing ..."
  createDirectoryIfMissing True dbPath
  mVar <- newMVar (0::Int)
  hdb <- HDB.new
  HDB.open hdb (M.tcDBPath dbPath) [HDB.OCREAT, HDB.OWRITER]
  TD.withIDB (M.tdDBPath dbPath) [TD.OCREAT, TD.OWRITER] $ \idb -> do
    flip readDirectoryWith targetPath $ \path ->
      if ".html" `isSuffixOf` path then
        do n <- takeMVar mVar
           contents <- readFile path
           putStrLn $ show n ++ ":" ++ path
           let bodyString = bodyText contents
           IDB.put idb (fromIntegral n :: Int64) (C8.pack bodyString)
           HDB.put hdb ("url:" ++ show n) path
           HDB.put hdb ("text:" ++ show n) bodyString
           putMVar mVar (n+1)
      else return ()
  HDB.close hdb
  putStrLn "done"

bodyText :: String -> String
bodyText html = parse html
  where
    parse = innerText . join . sections (~== (TagOpen "body" [])) . parseTags
