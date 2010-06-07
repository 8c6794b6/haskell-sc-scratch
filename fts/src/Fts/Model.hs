{-# LANGUAGE OverloadedStrings,
             PackageImports
  #-}
------------------------------------------------------------------------------
-- |
-- Database wrapper things.
--

module Fts.Model where

import "monads-fd" Control.Monad.Trans ( liftIO )
import Data.ByteString ( ByteString )
import Database.TokyoCabinet
    ( TCM
    , HDB )
import Database.TokyoDystopia 
    ( TDM
    , TDDB
    , IDB
    , OpenMode(..)
    , GetMode(..) )
import qualified Data.ByteString.Char8 as B
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoDystopia as TD
import qualified Database.TokyoDystopia.IDB as IDB


-- | Path to TokyoDystopia index database.
tdDBPath :: String
tdDBPath = "/home/atsuro/repos/haskell-sc-scratch/fts/db/casket"


-- | Path to TokyoCabinet key value storage.
tcDBPath :: String
tcDBPath = "/home/atsuro/repos/haskell-sc-scratch/fts/db/db.tch"


data SearchResult = SearchResult
    { srUrl :: ByteString
    , srText :: ByteString
    } deriving (Eq, Show)


noRecord :: SearchResult
noRecord = SearchResult B.empty B.empty


search :: ByteString -> IO [ByteString]
search query = TD.runTDM $ do
  db <- TD.new :: TDM IDB
  TD.open db tdDBPath [OREADER]
  ids <- liftIO $ IDB.search2 db (B.unpack query)
  TD.close db >> TD.del db
  return $ fmap (B.pack . show) ids


getResults :: [ByteString] -> IO [SearchResult]
getResults keys = TC.runTCM $ do
    db <- TC.new :: TCM HDB
    TC.open db tcDBPath [OREADER]
    vs <- mapM (f db) keys
    TC.close db
    return vs
 where 
   f db key = do
     url <- TC.get db $ B.append "url:" key
     txt <- TC.get db $ B.append "text:" key
     return $ case (url,txt) of
                (Just u, Just t) -> SearchResult u t
                _                -> noRecord

