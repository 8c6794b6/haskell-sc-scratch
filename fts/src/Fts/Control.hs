{-# LANGUAGE OverloadedStrings,
             PackageImports #-}

module Fts.Control where

import Control.Concurrent ( MVar, readMVar, modifyMVar_ )
import "monads-fd" Control.Monad.Trans ( liftIO )
import Snap.Types
    ( Snap
    , ifTop
    , writeBS
    , getParam )
import Text.Templating.Heist ( TemplateState )
import Fts.Model ( SearchResult(..) )

import qualified Snap.Types as ST
import qualified Snap.Util.FileServe as FS
import qualified Text.Templating.Heist as HE
import qualified Text.Templating.Heist.Splices as HES 
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

import qualified Fts.Model as M
import qualified Fts.View as V

-- | Just show input interface, without results.
noQuery :: Snap ()
noQuery = ifTop $ writeBS "hello world"

-- | Show first page of search results.
queryPhrase :: MVar (TemplateState Snap) -> Snap ()
queryPhrase tsMVar = do
  -- q <- getParam "q"
  req <- ST.getRequest
  let q = ST.rqParam "q" req
  case q of
    Just q' -> do
           keys <- liftIO $ M.search (head q')
           vals <- liftIO $ M.getResults keys
           ts <- liftIO $ readMVar tsMVar
           let ts' = HE.bindSplice "results" (V.mkResults vals) $ ts
           ST.modifyResponse $ ST.setContentType "text/html"
           maybe ST.pass writeBS =<< HE.renderTemplate ts' "search"
    Nothing -> do
           ts <- liftIO $ readMVar tsMVar
           maybe ST.pass writeBS =<< HE.renderTemplate ts "search"
           -- liftIO (putStrLn "no query" ) >> writeBS ""

-- | Show results with specifying page.
queryPhrasePage :: Snap ()
queryPhrasePage = undefined
