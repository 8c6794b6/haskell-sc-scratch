{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/WebRoutes.html>

-}
module Main where

import Prelude hiding (head, id, (.))

import Control.Category (Category(..))
import Control.Monad (msum)
import Data.Data (Data, Typeable)
import Data.Monoid (mconcat)

import Happstack.Server
  (Response,ServerPartT,ok,toResponse,simpleHTTP,nullConf,seeOther,dir,notFound)
import Text.Blaze.Html5 ((!),html,head,body,title,p,toHtml,toValue,ol,li,a)
import Text.Blaze.Html5.Attributes (href)
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes
  (PathInfo(..),RouteT,showURL,runRouteT,Site(..),setDefault,mkSitePI)
import Web.Routes.TH (derivePathInfo)
import Web.Routes.Happstack (implSite)
import Web.Routes.Boomerang

newtype ArticleId = ArticleId {unArticleId :: Int}
  deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

data Sitemap
  = Home
  | Article ArticleId
  | UserOverview
  | UserDetail Int String
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

-- Manual definition of functions for router.
--
-- rHome :: PrinterParser e tok a (Sitemap :- a)
-- rHome = xpure (Home :-) $ \res -> case res of 
--     Home :- h -> Just h
--     _         -> Nothing
--
-- rArticle :: PrinterParser e tok (ArticleId :- a) (Sitemap :- a)
-- rArticle = xpure (arg (:-) Article) $ \res ->  case res of 
--     (Article a) :- x -> Just (a :- x) 
--     _                -> Nothing
--    
-- rUserOverview :: PrinterParser e tok a (Sitemap :- a)
-- rUserOverview = xpure (UserOverview :-) $ \res -> case res of
--   UserOverview :- h -> Just h
--   _                 -> Nothing
--  
-- rUserDetail :: PrinterParser e tok (Int :- (String :- r)) (Sitemap :- r)
-- rUserDetail = xpure (arg (arg (:-)) UserDetail) $ \res -> case res of
--   UserDetail i n :- r -> Just (i :- n :- r)
--   _                   -> Nothing
--

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir "favicon.ico" $ notFound (toResponse ())
       , implSite "http://localhost:8000" "/route/" site
       , seeOther "/route/" (toResponse ()) ]

site :: Site Sitemap (ServerPartT IO Response)
site = setDefault Home $ boomerangSite (runRouteT route) sitemap

sitemap :: Router Sitemap
sitemap =
  (rHome <>
   rArticle . (lit "article" </> articleId) <>
   lit "users" . (rUserOverview <>
                  rUserDetail </> int . lit "-" . anyString))

articleId :: Router ArticleId
articleId = xmaph ArticleId (Just . unArticleId) int

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
  Home -> homePage
  Article aId -> articlePage aId
  UserOverview -> userOverviewPage
  UserDetail uId name -> userDetailPage uId name

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = do
  articles <- mapM mkArticle [ArticleId 1 .. ArticleId 10]
  userOverview <- showURL UserOverview
  ok $ toResponse $
    html $ do
      head $ title $ toHtml "Web routes boomerang"
      body $ do
        a ! href (toValue userOverview) $ toHtml "User overview"
        ol $ mconcat articles
  where
    mkArticle aId = do
      url <- showURL (Article aId)
      return $ li $ a ! href (toValue url) $
        toHtml $ "Article " ++ (show $ unArticleId aId)

articlePage :: ArticleId -> RouteT Sitemap (ServerPartT IO) Response
articlePage (ArticleId aId) = do
  homeURL <- showURL Home
  ok $ toResponse $
    html $ do
      head $ title $ toHtml $ "Article " ++ show aId
      body $ do
        p $ toHtml $ "You are now reading article " ++ show aId
        p $ do toHtml "Click "
               a ! href (toValue homeURL) $ toHtml "here"
               toHtml " to return home."

userOverviewPage :: RouteT Sitemap (ServerPartT IO) Response
userOverviewPage = do
  users <- mapM mkUser [1..10]
  ok $ toResponse $
    html $ do
      head $ title $ toHtml "Our Users"
      body $ do
        ol $ mconcat users
  where
    mkUser userId = do
      url <- showURL (UserDetail userId ("user " ++ show userId))
      return $ li $ a ! href (toValue url) $
        toHtml $ "User " ++ show userId

userDetailPage :: Int -> String -> RouteT Sitemap (ServerPartT IO) Response
userDetailPage uId uName = do
  homeURL <- showURL Home
  ok $ toResponse $
    html $ do
      head $ title $ toHtml $ "User " ++ uName
      body $ do
        p $ toHtml $ "You are now viewing user detail page for " ++ uName
        p $ do
          toHtml "Click "
          a ! href (toValue homeURL) $ toHtml "here"
          toHtml " to return home."
