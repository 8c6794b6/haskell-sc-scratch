{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
------------------------------------------------------------------------------
-- |
-- Database wrapper things.
--

module Fts.Model where

import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B

import "monads-fd" Control.Monad.Trans ( liftIO )
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
tdDBPath :: String
tdDBPath = "/home/atsuro/repos/git/haskell-sc-scratch/fts/db/casket"

-- | Path to TokyoCabinet key value storage.
tcDBPath :: String
tcDBPath = "/home/atsuro/repos/git/haskell-sc-scratch/fts/db/db.tch"

-- | Data type for search result.
--
-- Contains url and excerpt document body.
data SearchResult = SearchResult
    { srUrl :: ByteString
    , srText :: ByteString
    } deriving (Eq, Show)

-- | Empty result.
noRecord :: SearchResult
noRecord = SearchResult B.empty B.empty

-- | Searches given query.
search :: ByteString -> IO [ByteString]
search query = TD.runTDM $ do
  db <- TD.new :: TDM IDB
  TD.open db tdDBPath [OREADER]
  ids <- liftIO $ IDB.search2 db (B.unpack query)
  TD.close db >> TD.del db
  return $ fmap (B.pack . show) ids

-- | Retrieve document data from tokyocabinet database.
getResults :: Int -> Int -> [ByteString] -> IO [SearchResult]
getResults lim offset keys = TC.runTCM $ do
    db <- TC.new :: TCM HDB
    TC.open db tcDBPath [OREADER]
    vs <- mapM (f db) (take lim $ drop offset $ keys)
    TC.close db
    return vs
 where
   f db key = do
     url <- TC.get db $ B.append "url:" key
     txt <- TC.get db $ B.append "text:" key
     return $ case (url,txt) of
                (Just u, Just t) -> SearchResult u t
                _                -> noRecord
