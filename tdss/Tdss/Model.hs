{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Tdss.Model where

import Data.ByteString ( ByteString )
import System.FilePath ((</>))
import qualified Data.ByteString.Char8 as B


import Control.Monad.Trans ( liftIO )
import Database.TokyoCabinet
    ( TCM
    , HDB )
import Database.TokyoDystopia
    ( TDM
    , IDB
    , OpenMode(..) )
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoDystopia as TD
import qualified Database.TokyoDystopia.IDB as IDB

-- | Path to TokyoDystopia index database.
tdDBPath :: FilePath -> FilePath
tdDBPath root = root </> "casket"

-- | Path to TokyoCabinet key value storage.
tcDBPath :: FilePath -> FilePath
tcDBPath root = root </> "db.tch"

-- | Data type for search result.
--
-- Contains url and excerpt document body.
data SearchResult = SearchResult
    { srUrl :: ByteString
    , srText :: ByteString
    } deriving (Eq, Show)

-- | Searches given query.
search :: FilePath -> ByteString -> IO [ByteString]
search dbRoot query = TD.runTDM $ do
  db <- TD.new :: TDM IDB
  TD.open db (tdDBPath dbRoot) [OREADER]
  ids <- liftIO $ IDB.search2 db (B.unpack query)
  TD.close db >> TD.del db
  return $ fmap (B.pack . show) ids

-- | Retrieve document data from tokyocabinet database.
getResults :: FilePath -> Int -> Int -> [ByteString] -> IO [SearchResult]
getResults dbRoot lim offset keys = TC.runTCM $ do
    db <- TC.new :: TCM HDB
    TC.open db (tcDBPath dbRoot) [OREADER]
    vs <- mapM (f db) (take lim $ drop offset $ keys)
    TC.close db
    return vs
 where
   f db key = do
     url <- TC.get db $ B.append "url:" key
     txt <- TC.get db $ B.append "text:" key
     return $ case (url,txt) of
                (Just u, Just t) -> SearchResult u t
                _                -> SearchResult B.empty B.empty
