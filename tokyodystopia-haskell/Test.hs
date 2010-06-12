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
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.List as TCL
import qualified Database.TokyoDystopia as TD
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB


main :: IO ()
main = do

  idb <- TD.runTDM $ do
           a <- test_read_idb
           b <- test_write_idb
           c <- test_read_idb_2 1
           d <- test_search_idb "united"
           return (a,b,c,d)
  putStrLn "idb tests:" >> print idb

  qdb <- TD.runTDM $ do
           a <- test_write_qdb
           b <- test_read_qdb
           c <- test_search_qdb "a"
           return (a, b, c)
  putStrLn "qdb tests:" >> print qdb

  jdb <- TD.runTDM $ do
           a <- test_write_jdb
           b <- test_read_jdb
           c <- test_search_qdb "a"
           return (a, b, c)
  putStrLn "jdb tests:" >> print jdb


------------------------------------------------------------------------------
--
-- IDB
--
------------------------------------------------------------------------------

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
  TD.open db "test/casket" [OCREAT, OWRITER]
  res <- mapM (uncurry $ TD.put db)
       [ (1, "foo")
       , (2, "bar")
       , (3, "buzz") ]
  TD.close db >> TD.del db
  return $ and res

test_read_idb_2 :: Int64 -> TDM (Maybe ByteString)
test_read_idb_2 key = do
  db <- TD.new :: TDM IDB
  TD.open db "test/casket" [OREADER]
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


------------------------------------------------------------------------------
--
-- QDB
--
------------------------------------------------------------------------------

test_write_qdb :: TDM Bool
test_write_qdb = do
  db <- TD.new :: TDM QDB
  TD.open db "test/casket.tcq" [OWRITER, OCREAT]
  res <- mapM (uncurry $ TD.put db)
         [(1, "hello"),
          (2, "haskell"),
          (3, "tokyo"),
          (4, "dystopia"),
          (5, "qdb")]
  TD.close db >> TD.del db
  return $ and res

test_read_qdb :: TDM (Maybe ByteString)
test_read_qdb = do
  db <- TD.new :: TDM QDB
  TD.open db "test/casket.tcq" [OREADER]
  res <- TD.get db 1
  TD.close db >> TD.del db
  return $ res

test_search_qdb :: String -> TDM [Int64]
test_search_qdb query = do
  db <- TD.new :: TDM QDB
  TD.open db "test/casket.tcq" [OREADER]
  res <- TD.search db query [GMSUBSTR]
  TD.close db >> TD.del db
  return res


------------------------------------------------------------------------------
--
-- JDB
--
------------------------------------------------------------------------------

test_write_jdb :: TDM Bool
test_write_jdb = do

  db <- TD.new :: TDM JDB
  TD.open db "test/laputa" [OCREAT, OWRITER]

  l1 <- liftIO (TCL.new :: IO (TCL.List ByteString))
  l2 <- liftIO (TCL.new :: IO (TCL.List ByteString))
  l3 <- liftIO (TCL.new :: IO (TCL.List ByteString))

  liftIO $ mapM_ (TCL.push l1 . C8.pack) ["apple", "ant", "antenna"] >>
           mapM_ (TCL.push l2 . C8.pack) ["banana", "bear", "bubble"] >>
           mapM_ (TCL.push l3 . C8.pack) ["cherry", "chair", "chocolate"]

  r1 <- TD.put db 1 l1
  r2 <- TD.put db 2 l2
  r3 <- TD.put db 3 l3
  
  TD.close db >> TD.del db >> liftIO (mapM_ TCL.delete [l1,l2,l3])

  return $ and [r1, r2, r3]


test_read_jdb :: TDM ByteString
test_read_jdb = do
  db <- TD.new :: TDM JDB
  TD.open db "test/laputa" [OREADER]
  res <- TD.get db 2
  res' <- liftIO $ maybe (return B.empty) TCL.dump res
  TD.close db >> TD.del db
  return res' 


test_search_jdb :: String -> TDM [Int64]
test_search_jdb q = do
  db <- TD.new :: TDM JDB
  TD.open db "test/laputa" [OREADER]
  res <- TD.search db q [GMSUBSTR]
  TD.close db >> TD.del db
  return res


------------------------------------------------------------------------------
--
-- WDB
--
------------------------------------------------------------------------------

test_write_wdb :: TDM Bool
test_write_wdb = do
  undefined

test_read_wdb :: TDM [ByteString]
test_read_wdb = do
  undefined

test_search_wdb :: String -> TDM [Int64]
test_search_wdb q = do
  undefined