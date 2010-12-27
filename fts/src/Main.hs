{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main where

import System
import Control.Applicative ((<|>))
import Control.Concurrent ( MVar, newMVar, readMVar )
import qualified Data.ByteString.Char8 as C8

import "monads-fd" Control.Monad.Trans ( liftIO )
import Snap.Http.Server
import Snap.Types
import Text.Templating.Heist ( TemplateState )
import qualified Snap.Util.FileServe as FS
import qualified Text.Templating.Heist as HE

import qualified Fts.Control as C


main :: IO ()
main = do
    args <- getArgs
    ets <- HE.loadTemplates templatePath (HE.emptyTemplateState "")
    let ts = either error id ets
        port | null args = 8000
             | otherwise = read $ head args
        conf = addListen (ListenHttp "127.0.0.1" port) defaultConfig
    tsMVar <- newMVar ts
    httpServe conf (site tsMVar)

-- | Main url mapping.
site :: MVar (TemplateState Snap) -> Snap ()
site tsMVar =
  FS.fileServe "./"
  <|> route [("", C.queryPhrase tsMVar)]
  <|> templateServe tsMVar

-- | Absolute path to template directory.
templatePath :: FilePath
templatePath = "/home/atsuro/repos/git/haskell-sc-scratch/fts/templates"

-- | Serves templates with state in MVar.
templateServe :: MVar (TemplateState Snap) -> Snap ()
templateServe tsMVar = do
  ts <- liftIO $ readMVar tsMVar
  urlPath <- return . maybe "search" id . urlDecode . C8.pack =<< FS.getSafePath
  maybe pass writeBS =<< HE.renderTemplate ts urlPath
  modifyResponse $ setContentType "text/html"
