{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Simple Create-Read-Update-Delete example using Happstack with builtin macid.
Enable editing article data. Each article has title and contents.

-}
module Main where

import Prelude hiding (head, id, (.))

import Control.Applicative (Applicative(..), Alternative(..),(<$>))
import Control.Category (Category(..))
import Control.Exception (bracket)
import Control.Monad (forM, forM_, msum, when)
import Control.Monad.Reader (ask)
import Control.Monad.State (get,put,modify)
import Data.Data (Data, Typeable)
import Data.Monoid (mconcat)

import Happstack.Server hiding (body, method)
import Happstack.Server.RqData (getDataFn)
import Happstack.State
import Text.Blaze.Html5
  (Html,(!),html,head,body,title,p,toHtml,toValue,a,textarea,form,input,ul,li)
import Text.Blaze.Html5.Attributes
  (href,type_,enctype,action,name,action,method,value)
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes
import Web.Routes.Happstack (implSite)
import Web.Routes.Boomerang hiding (rList)

import qualified Data.IntMap as IM

data Article = Article
  { atcId :: Int
  , atcTitle :: String
  , atcContents :: String
  } deriving (Eq, Show, Ord, Data, Typeable)

instance Version Article
$(deriveSerialize ''Article)

newtype AppState = AppState
  { articles :: IM.IntMap Article
  } deriving (Eq, Show, Data, Typeable)

instance Version AppState
$(deriveSerialize ''AppState)

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState {articles = IM.empty}

peekArticle :: Query AppState (IM.IntMap Article)
peekArticle = articles <$> ask

updateArticle :: Article -> Update AppState ()
updateArticle art@(Article aI aT aC) = do
  AppState amap <- get
  let amap' | aI < 0    = IM.insert newId (art {atcId=newId}) amap
            | otherwise = IM.insert aI art amap
      newId | IM.size amap == 0 = 1
            | otherwise         = succ $ fst $ IM.findMax amap
  put $ AppState amap'

deleteArticle :: Int -> Update AppState ()
deleteArticle aId = modify $ \(AppState amap) -> AppState $ IM.delete aId amap

$(mkMethods ''AppState ['updateArticle, 'peekArticle,'deleteArticle])

data Sitemap
  = AList
  | ADetail Int
  | ANew
  | AEdit Int
  | ACreate
  | AUpdate Int
  | ADelete Int

$(derivePrinterParsers ''Sitemap)

site :: Site Sitemap (ServerPartT IO Response)
site = setDefault AList $ boomerangSite (runRouteT route) urlmap

urlmap :: Router Sitemap
urlmap =
  rAList <>
  rADetail . int <>
  rANew . (lit "new") <>
  rACreate . (lit "create") <>
  rAEdit . (lit "edit" </> int) <>
  rAUpdate . (lit "update" </> int) <>
  rADelete . (lit "delete" </> int)

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
  AList       -> listPage
  ADetail aId -> detailPage aId
  ANew        -> newPage
  AEdit aId   -> editPage aId
  ACreate     -> createPage
  AUpdate aId -> updatePage aId
  ADelete aId -> deletePage aId

main :: IO ()
main =
  bracket (startSystemState (Proxy :: Proxy AppState))
    createCheckPointAndShutdown $ const $ simpleHTTP nullConf $ msum
      [ dir "favicon.ico" $ notFound (toResponse ())
      , implSite "http://localhost:8000" "/article/" site
      , seeOther "/article/" (toResponse ()) ]
  where
    createCheckPointAndShutdown ctrl = do
      createCheckpoint ctrl
      shutdownSystem ctrl

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" (10*10^6) 10000 10000

type CrudPage = RouteT Sitemap (ServerPartT IO) Response

listPage :: CrudPage
listPage = do
  newURL <- showURL ANew
  amap <- query PeekArticle
  links <- mapM mkLink (IM.elems amap)
  ok $ toResponse $ html $ do
    head $ title $ toHtml "Article list"
    body $ do
      p $ toHtml $ "Articles: " ++ show (IM.size amap) ++ "."
      ul $ mconcat links
      p $ a ! href (toValue newURL) $ toHtml "create new article"
  where
    mkLink atc = do
      url <- showURL (ADetail (atcId atc))
      return $ li $ a ! href (toValue url) $ toHtml $ atcTitle atc

detailPage :: Int -> CrudPage
detailPage aId = do
  amap <- query PeekArticle
  listURL <- showURL AList
  editURL <- showURL (AEdit aId)
  deleteURL <- showURL (ADelete aId)
  case IM.lookup aId amap of
    Nothing  ->
      badRequest $ toResponse $ "No such article."
    Just atc -> do
      ok $ toResponse $ html $ do
        head $ title $ toHtml $ atcTitle atc
        body $ do
          p $ toHtml $ "title: " ++ atcTitle atc
          p $ toHtml $ "contents: " ++ atcContents atc
          p $ a ! href (toValue editURL) $ toHtml "edit"
          p $ a ! href (toValue deleteURL) $ toHtml "delete"
          p $ a ! href (toValue listURL) $ toHtml "back to list"

newPage :: CrudPage
newPage = do
  createURL <- showURL ACreate
  ok $ toResponse $ html $ do
    head $ title $ toHtml "New article"
    editTpl createURL "" ""

editPage :: Int -> CrudPage
editPage aId = do
  updateURL <- showURL (AUpdate aId)
  atc <- IM.lookup aId <$> query PeekArticle
  case atc of
    Nothing ->
      badRequest $ toResponse $ "No such article."
    Just atc' -> do
      ok $ toResponse $ html $ do
        head $ title $ toHtml "Edit article"
        editTpl updateURL (atcTitle atc') (atcContents atc')

editTpl :: String -> String -> String -> Html
editTpl actURL titleV contentV =
  body $ do
    form !
      enctype (toValue "multipart/form-data") !
      method (toValue "POST") !
      action (toValue actURL) $ do
        input !
          type_ (toValue "text") !
          name (toValue "title") !
          value (toValue titleV)
        textarea (toHtml contentV) !
          name (toValue "contents")
        input !
          type_ (toValue "submit") !
          value (toValue "done")

createPage :: CrudPage
createPage = updatePage (-1)

updatePage :: Int -> CrudPage
updatePage aId = do
  decodeBody myPolicy
  r <- getDataFn $ (,) <$> look "title" <*> look "contents"
  case r of
    Left e -> badRequest $ toResponse $ unlines e
    Right (ttl,cnt) -> do
      update (UpdateArticle (Article aId ttl cnt))
      listPage

deletePage :: Int -> CrudPage
deletePage aId = update (DeleteArticle aId) >> listPage
