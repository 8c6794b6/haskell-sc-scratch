{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Fts.Command.Serve where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, newMVar, readMVar)
import qualified Data.ByteString.Char8 as C8

import "monads-fd" Control.Monad.Trans (liftIO)
import Snap.Http.Server
import Snap.Types
import Text.Templating.Heist (TemplateState)
import qualified Snap.Util.FileServe as FS
import qualified Text.Templating.Heist as HE

import qualified Fts.Control as C

-- | Serve the site.
run :: Int      -- ^ Port number
    -> FilePath -- ^ Database path
    -> FilePath -- ^ Template path
    -> IO ()
run pNum dPath tPath = do
  ets <- HE.loadTemplates tPath (HE.emptyTemplateState "")
  let ts = either error id ets
      conf = addListen (ListenHttp "127.0.0.1" pNum) defaultConfig
  tsMVar <- newMVar ts
  httpServe conf (site dPath tsMVar)

-- | Main url mapping.
site :: FilePath -> MVar (TemplateState Snap) -> Snap ()
site db tsMVar =
  FS.fileServe "./"
  <|> route [("", C.queryPhrase db tsMVar)]
  <|> templateServe tsMVar

-- | Serves templates with state in MVar.
templateServe :: MVar (TemplateState Snap) -> Snap ()
templateServe tsMVar = do
  ts <- liftIO $ readMVar tsMVar
  urlPath <- return . maybe "search" id . urlDecode . C8.pack =<< FS.getSafePath
  maybe pass writeBS =<< HE.renderTemplate ts urlPath
  modifyResponse $ setContentType "text/html"
