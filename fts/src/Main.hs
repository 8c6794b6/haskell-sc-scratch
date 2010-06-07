{-# LANGUAGE OverloadedStrings,
             PackageImports
  #-}
module Main where

import System

import Control.Applicative ((<|>))
import Control.Concurrent ( MVar, newMVar, readMVar )
import Control.Monad (liftM)
import "monads-fd" Control.Monad.Trans ( liftIO )
import Snap.Http.Server
import Snap.Types

import Text.Templating.Heist ( TemplateState )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Snap.Util.FileServe as FS
import qualified Text.Templating.Heist as HE

import qualified Fts.Control as C 


main :: IO ()
main = do
    args <- getArgs
    -- ets <- HE.loadTemplates "templates" HE.emptyTemplateState
    ets <- HE.loadTemplates templatePath HE.emptyTemplateState
    let ts = either error id ets
        port = case args of
                   []  -> 8000
                   p:_ -> read p
    tsMVar <- newMVar ts
    httpServe "*" port "myserver"
        (Just "access.log")
        (Just "error.log")
        (site tsMVar)


-- | Main url mapping. 
site :: MVar (TemplateState Snap) -> Snap ()
site tsMVar = 
    route [("search", C.queryPhrase tsMVar)]
    <|> templateServe tsMVar
    <|> FS.fileServe "./"


-- | Absolute path to template directory.
templatePath :: FilePath
templatePath = "/home/atsuro/repos/haskell-sc-scratch/fts/fts/templates"


-- | Serves templates with state in MVar. 
templateServe :: MVar (TemplateState Snap) -> Snap ()
templateServe tsMVar = do
  ts <- liftIO $ readMVar tsMVar
  maybe pass writeBS =<< HE.renderTemplate ts . C8.pack =<< FS.getSafePath
  modifyResponse $ setContentType "text/html"

