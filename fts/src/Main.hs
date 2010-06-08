{-# LANGUAGE OverloadedStrings,
             PackageImports
  #-}
module Main where

import System
import System.Directory ( doesFileExist )
import System.FilePath ( (</>) )
import System.FilePath.Posix ( takeFileName, takeExtensions )
import Control.Applicative ((<|>))
import Control.Concurrent ( MVar, newMVar, readMVar )
import Control.Monad ( liftM, unless )
import Data.ByteString ( ByteString )
import Data.Maybe ( fromMaybe )
import "monads-fd" Control.Monad.Trans ( liftIO )
import Snap.Http.Server
import Snap.Types

import Text.Templating.Heist ( TemplateState )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
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
    <|> fileServe "./"


-- | Absolute path to template directory.
templatePath :: FilePath
templatePath = "/home/atsuro/repos/haskell-sc-scratch/fts/templates"


-- | Serves templates with state in MVar. 
templateServe :: MVar (TemplateState Snap) -> Snap ()
templateServe tsMVar = do
  ts <- liftIO $ readMVar tsMVar
  urlPath <- return . maybe "search" id . urlDecode . C8.pack =<< FS.getSafePath
  maybe pass writeBS =<< HE.renderTemplate ts urlPath
  modifyResponse $ setContentType "text/html"

-- | Reimplementation of Snap.Util.FileServe.fileServe, for paths including
-- url encoded string.
fileServe :: FilePath -> Snap ()
fileServe root = do
  urlPath <- return . C8.unpack . maybe "" id . urlDecode . C8.pack =<< 
             FS.getSafePath
  let fp = root </> urlPath 
  liftIO (doesFileExist fp) >>= flip unless pass
  let fn = takeFileName urlPath
      mime = fileType FS.defaultMimeTypes fn
  FS.fileServeSingle' mime fp
  where
    fileType :: FS.MimeMap -> String -> ByteString 
    fileType mm f = 
        if null ext 
           then defaultMimeType
           else fromMaybe (fileType mm (drop 1 ext)) mbe
      where
        ext = takeExtensions f
        mbe = Map.lookup ext mm
    
    defaultMimeType :: ByteString 
    defaultMimeType = "application/octet-stream"