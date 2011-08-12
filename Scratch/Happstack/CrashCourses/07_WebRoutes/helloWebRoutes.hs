{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/WebRoutes.html>

-}
module Main where

import Prelude hiding (head)

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (msum)
import Data.Data (Data, Typeable)
import Data.Monoid (mconcat)

import Happstack.Server 
  (Response, ServerPartT, ok, toResponse, simpleHTTP, nullConf, seeOther
  ,dir, notFound)
import Text.Blaze.Html5 
  ((!), html, head, body, title, p, toHtml, toValue, ol, li, a)
import Text.Blaze.Html5.Attributes (href)  
import Web.Routes
  (PathInfo(..), RouteT, showURL, runRouteT, Site(..), setDefault, mkSitePI)
import Web.Routes.TH (derivePathInfo)
import Web.Routes.PathInfo (segment)
import Web.Routes.Happstack (implSite)

newtype ArticleId = ArticleId {unArticleId :: Int}
  deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)
           
data Sitemap
  = Home
  | Article ArticleId
    deriving (Eq, Ord, Read, Show, Data, Typeable)
             
$(derivePathInfo ''Sitemap)

-- Manually defining instance of PathInfo.
-- 
-- instance PathInfo Sitemap where
--   toPathSegments sm = case sm of
--     Home        -> ["Home"]
--     Article aId -> ["Article"] ++ toPathSegments aId
--   fromPathSegments = 
--     (segment "Home" *> pure Home) <|>
--     (segment "Article" *> pure Article <*> fromPathSegments)

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
  Home        -> homePage
  Article aId -> articlePage aId
  
homePage :: RouteT Sitemap (ServerPartT IO) Response  
homePage = do
  articles <- mapM mkArticle [(ArticleId 1) .. (ArticleId 10)]
  ok $ toResponse $ 
    html $ do
      head $ title $ toHtml "Welcome Home"
      body $ do
        ol $ mconcat articles
  where
    mkArticle articleId = do
      url <- showURL (Article articleId)
      return $ li $ a ! href (toValue url) $ 
        toHtml $ "Article " ++ (show $ unArticleId articleId)
        
articlePage :: ArticleId -> RouteT Sitemap (ServerPartT IO) Response
articlePage (ArticleId articleId) = do
  homeURL <- showURL Home
  ok $ toResponse $ 
    html $ do
      head $ title $ toHtml $ "Article " ++ show articleId
      body $ do
        p $ toHtml $ "You are now reading article " ++ show articleId
        p $ do
          toHtml $ "Click "
          a ! href (toValue homeURL) $ toHtml "here"
          toHtml " to return home."
  
site :: Site Sitemap (ServerPartT IO Response)  
site = setDefault Home $ mkSitePI (runRouteT route)

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir "favicon.ico" $ notFound (toResponse ())
       , implSite "http://localhost:8000" "/route/" site
       , seeOther "/route/" (toResponse ()) ]