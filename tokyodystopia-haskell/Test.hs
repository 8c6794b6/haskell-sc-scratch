{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Main where

import Control.Monad
import "monads-fd" Control.Monad.Trans ( liftIO )
import Data.Int ( Int64 )
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

import Database.TokyoDystopia
    ( TDM
    , OpenMode(..)
    , GetMode(..)
    , IDB
    , JDB
    , QDB )
import qualified Database.TokyoDystopia as TD
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB

main :: IO ()
main = do
  res <- TD.runTDM $ do 
           a <- test_read_idb
           b <- test_write_idb 
           c <- test_read_idb_2 1
           d <- test_search_idb "united"
           return (a,b,c,d) 
  print res

test_read_idb :: TDM (Maybe ByteString)
test_read_idb = do
  db <- TD.new :: TDM IDB
  TD.open db "/home/atsuro/tmp/tokyo/dystopia/dyst/casket" [OWRITER]
  val <- TD.get db 1
  TD.close db >> TD.del db
  return val

test_write_idb :: TDM Bool
test_write_idb = do
  db <- TD.new :: TDM IDB
  TD.open db "casket" [OCREAT, OWRITER]
  res <- mapM (uncurry $ TD.put db)
       [ (1, "foo")
       , (2, "bar")
       , (3, "buzz") ]
  TD.close db >> TD.del db
  return $ and res

test_read_idb_2 :: Int64 -> TDM (Maybe ByteString)
test_read_idb_2 key = do
  db <- TD.new :: TDM IDB
  TD.open db "casket" [OREADER]
  res <- TD.get db key
  TD.close db >> TD.del db
  return res

test_search_idb :: String -> TDM [(Int64, ByteString)]
test_search_idb query = do
  db <- (TD.new :: TDM IDB)
  TD.open db "/home/atsuro/tmp/tokyo/dystopia/dyst/casket" [OREADER]
  ks <- TD.search db query [GMSUBSTR]
  res <- mapM (\k -> TD.get db k >>= \(Just v) -> return (k,v)) ks
  TD.close db >> TD.del db
  return res

test_write_qdb :: TDM Bool
test_write_qdb = do
  db <- TD.new :: TDM QDB
  TD.open db "qasket" [OWRITER, OCREAT]
  res <- mapM (uncurry $ TD.put db) 
         [(1, "hello"),
          (2, "haskell"),
          (3, "tokyo"),
          (4, "dystopia"),
          (5, "qdb")]
  TD.close db >> TD.del db
  return $ and res

test_search_qdb :: String -> TDM [Int64]
test_search_qdb query = do
  db <- TD.new :: TDM QDB
  TD.open db "qasket" [OREADER]
  res <- TD.search db query [GMSUBSTR] 
  TD.close db >> TD.del db
  return res