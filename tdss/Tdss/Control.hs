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
module Tdss.Control where

import Control.Concurrent (MVar, readMVar)
import qualified Data.ByteString.Char8 as C8

import Blaze.ByteString.Builder (toByteString)
import Control.Monad.Trans (liftIO)
import Snap.Types (Snap , writeBS)
import Text.Templating.Heist ( TemplateState )
import qualified Snap.Types as ST
import qualified Text.Templating.Heist as HE

import qualified Tdss.Model as M
import qualified Tdss.View as V

-- | Show first page of search results.
queryPhrase :: FilePath -> MVar (TemplateState Snap) -> Snap ()
queryPhrase dbPath tsMVar = do
  req <- ST.getRequest
  ST.modifyResponse $ ST.setContentType "text/html"
  let q = ST.rqParam "q" req
      p = ST.rqParam "p" req
      p' = maybe 1
           (\x -> if length x > 0 then read (C8.unpack $ head x) else 1)
           p
  ts <- liftIO $ readMVar tsMVar
  let ts' = HE.bindSplice "input_query" (V.mkInputQuery q) ts
  contents <- case q of
    Just (q':_) -> do
      keys <- liftIO $ M.search dbPath q'
      vals <- liftIO $ M.getResults dbPath V.perPage ((p'-1) * V.perPage) keys
      let ts'' = HE.bindSplice "results" (V.mkResults vals) .
                 HE.bindSplice "summary" (V.mkSummary keys req) .
                 HE.bindSplice "page_links" (V.mkPageLinks keys req) $
                 ts'
      HE.renderTemplate ts'' "search"
    _          -> HE.renderTemplate ts' "search"
  maybe ST.pass (writeBS . toByteString . fst) contents
