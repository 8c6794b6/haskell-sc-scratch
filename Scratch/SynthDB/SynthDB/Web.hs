{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Web interface to browse synthdef database.

-}
module SynthDB.Web where

import Prelude hiding ((.),id,div,head,span)

import Control.Applicative (Applicative(..), (<$>))
import Control.Category (Category(..))
import Control.Monad (msum,forM_,zipWithM_)
import Data.Monoid (mconcat)

import Control.Monad.Trans (liftIO)
import Happstack.Server hiding (body, method)
import Sound.SC3.Lepton.Parser
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir,id,title,form,style,span)
import Text.Boomerang.TH (derivePrinterParsers)
import Text.Pandoc
import Web.Routes
import Web.Routes.Happstack (implSite)
import Web.Routes.Boomerang

import qualified Data.ByteString.Char8 as C8

import SynthDB.Persist
import SynthDB.QHereDoc

data Sitemap
  = SSearch
  | SDetail String
  | SStat
  | SHelp
  | SAbout

$(derivePrinterParsers ''Sitemap)
rSSearch :: PrinterParser e tok r (Sitemap :- r)
rSDetail :: PrinterParser e tok (String :- r) (Sitemap :- r)
rSStat :: PrinterParser e tok r (Sitemap :- r)
rSHelp :: PrinterParser e tok r (Sitemap :- r)
rSAbout :: PrinterParser e tok r (Sitemap :- r)

instance ToHtml SynthDefFile where
  toHtml (SynthDefFile v defs) = do
    div ! class_ (toValue "synthdeffile") $ do
      div $ toHtml $ "version: " ++ show v
      div $ mconcat $ map toHtml defs

instance ToHtml SynthDef where
  toHtml (SynthDef n cs pvs pps ugs) = do
    div ! class_ (toValue "synthdef") $ do
      div $ toHtml $ "synthdef: " ++ n
      div $ do
        div ! class_ (toValue "parameters") $ do
          div $ toHtml "parameters"
          div $ do
            ul $ zipWithM_ mkPair pps [0..]
        div ! class_ (toValue "ugens") $ do
          div $ toHtml "ugens"
          div $ do
            ul $ zipWithM_ mkUS ugs [0..]
    where
      mkPair :: ParamPair -> Int -> Html
      mkPair pp idx
        | length pvs < pI =
          li $ toHtml $ show idx ++ ": " ++ pN ++ " (nan)"
        | otherwise      =
          li $ toHtml $ show idx ++ ": " ++ pN ++ " (" ++ show (pvs !! pI) ++ ")"
        where
          pI = fromIntegral $ ppIndex pp
          pN = ppName pp
      mkUS :: UGenSpec -> Int -> Html
      mkUS (UGenSpec un r sp is os) idx =
        li $ toHtml $
          show idx ++ ": " ++ mkUN un sp ++ " " ++ showRate r ++ " " ++
          unwords (map iSpec is) ++ " " ++
          unwords (map oSpec os)
      iSpec idx = case idx of
        IsConstant cix   ->
          "(" ++ show (cs !! fromIntegral cix) ++ ")"
        IsUGenOut ugi oix ->
          "(ugen " ++ show (fromIntegral ugi::Int) ++ ":" ++ show oix ++ ")"
      oSpec o = "(out:" ++ showRate o ++ ")"
      showRate r = case r of
        0 -> "ir"; 1 -> "kr"; 2 -> "ar"; 3 -> "dr"; _ -> ""

instance ToHtml Stat where
  toHtml (Stat nDef nUGen uMap pMap) = do
    div ! class_ (toValue "stat") $ do
      div ! class_ (toValue "stat_summary") $ do
        div ! class_ (toValue "stat_summary_name") $ do
          toHtml $ "Number of synthdefs: "
        div ! class_ (toValue "stat_summary_value") $ do
          toHtml $ show nDef
        div ! class_ (toValue "stat_summary_name") $ do
          toHtml $ "Number of UGens: "
        div ! class_ (toValue "stat_summary_value") $ do
          toHtml $ show nUGen
      div ! class_ (toValue "stat_ugen") $ do
        ul $ forM_ uMap $ \(k,v) -> do
          li $ do
            div ! class_ (toValue "stat_ugen_name") $ do
              toHtml $ k ++ " : "
            div ! class_ (toValue "stat_ugen_value") $ do
              toHtml $ show v
      div ! class_ (toValue "stat_param") $ do
        ul $ forM_ pMap $ \(k,v) -> do
          li $ do
            div ! class_ (toValue "stat_param_name") $ do
              toHtml $ k ++ " : "
            div ! class_ (toValue "stat_param_value") $ do
              toHtml $ show v

instance ToHtml Pandoc where
  toHtml = preEscapedString .
           writeHtmlString defaultWriterOptions {writerWrapText=False}

serve :: Int -> IO ()
serve pn = simpleHTTP nullConf {port=pn} $ msum
  [ dir "favicon.ico" $ notFound (toResponse ())
  , implSite ("http://localhost:" ++ show pn ++ "/") "" site
  , seeOther "" (toResponse ())
  ]

site :: Site Sitemap (ServerPartT IO Response)
site = setDefault SSearch $ boomerangSite (runRouteT route) urlmap

urlmap :: Router Sitemap
urlmap =
  rSSearch <>
  rSDetail . (lit "detail" </> rList anyChar) <>
  rSStat . (lit "stats") <>
  rSHelp . (lit "help") <>
  rSAbout . (lit "about")

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
  SSearch         -> listPage
  SDetail defName -> detailPage defName
  SStat           -> statPage
  SHelp           -> helpPage
  SAbout          -> aboutPage

headerTpl :: String -> String -> String -> String -> Html
headerTpl lURL sURL hURL aURL =
  div ! class_ (toValue "header")  $ do
    a ! href (toValue lURL) $ toHtml "search"
    toHtml " / "
    a ! href (toValue sURL) $ toHtml "stat"
    toHtml " / "
    a ! href (toValue hURL) $ toHtml "help"
    toHtml " / "
    a ! href (toValue aURL) $ toHtml "about"

headerHtml :: RouteT Sitemap (ServerPartT IO) Html
headerHtml =
  headerTpl <$>
  showURL SSearch <*> showURL SStat <*> showURL SHelp <*> showURL SAbout

listPage :: RouteT Sitemap (ServerPartT IO) Response
listPage = do
  qs <- getDataFn $ look "q"
  listURL <- showURL SSearch
  headerH <- headerHtml
  case qs of
    Left _   -> do
      ok $ toResponse $ html $ do
        head $ do
          title $ toHtml "synthdb - search"
          style ! type_ (toValue "text/css") $ css
        body $ do
          div ! class_ (toValue "wrapper") $ do
            headerH
            div ! class_ (toValue "searchform") $ inputForm listURL ""
    Right qs' -> do
      rs <- liftIO $ queryUGs qs'
      links <- mapM mkLink rs
      ok $ toResponse $ html $ do
        head $ do
          title $ toHtml $ "synthdb - search result for: " ++ qs'
          style ! type_ (toValue "text/css") $ css
        body $ do
          div ! class_ (toValue "wrapper") $ do
            headerH
            div ! class_ (toValue "searchform") $ do
              inputForm listURL qs'
            div $ ul $ mconcat links
  where
    inputForm actionURL lastVal = do
      form ! enctype (toValue "multipart/form-data") !
        method (toValue "GET") ! action (toValue actionURL) $ do
          input !
            type_ (toValue "text") !
            name (toValue "q") !
            size (toValue "40") !
            value (toValue lastVal) !
            autofocus (toValue "autofocus")
          input !
            type_ (toValue "submit") !
            value (toValue "search")
    mkLink sd = do
      let sdn = f (sdrName sd)
          f (SDName n) = C8.unpack n
      url <- showURL (SDetail sdn)
      return $ li $ a ! href (toValue url) $ toHtml sdn

detailPage :: String -> RouteT Sitemap (ServerPartT IO) Response
detailPage defName = do
  sd <- liftIO $ getDef defName
  headerH <- headerHtml
  ok $ toResponse $ html $ do
    head $ do
      title $ toHtml $ "synthdb - synthdef: " ++ (unwords $ map sdName $ sdDefs sd)
      style ! type_ (toValue "text/css") $ css
    body $ do
      div ! class_ (toValue "wrapper") $ do
        headerH
        toHtml sd

statPage :: RouteT Sitemap (ServerPartT IO) Response
statPage = do
  headerH <- headerHtml
  stat <- liftIO getStat
  ok $ toResponse $ html $ do
    head $ do
      title $ toHtml "synthdb - stat"
      style ! type_ (toValue "text/css") $ css
    body $ do
      div ! class_ (toValue "wrapper") $ do
        headerH
        toHtml stat

dumbPage :: String -> RouteT Sitemap (ServerPartT IO) Response
dumbPage mkd = do
  headerH <- headerHtml
  ok $ toResponse $ html $ do
    head $ do
      title $ toHtml "synthdb - help"
      style ! type_ (toValue "text/css") $ css
    body $ do
      div ! class_ (toValue "wrapper") $ do
        headerH
        toHtml $ readMarkdown defaultParserState mkd

aboutPage :: RouteT Sitemap (ServerPartT IO) Response
aboutPage = dumbPage [hereDoc|
synthdb - command line ui and web ui
------------------------------------
synthdb is a toolset for browsing synthdef files written by
[supercollider](http://supercollider.sourceforge.net)
and [hsc3](http://slavepianos.org/rd/?t=hsc3).

 This software was made as an exercise for using
[acid-state](http://acid-state.seize.it/) and
[ixset](http://hackage.haskell.org/package/ixset/).
Later it has been extended to have web ui with
[happstack](http://happstack.com/index.html).
|]

helpPage :: RouteT Sitemap (ServerPartT IO) Response
helpPage = dumbPage [hereDoc|
Initializing database
---------------------

To serve the site, initialize the database with:

    $ synthdb initialize

then from the same directory:

    $ synthdb serve -p 8000

and then access to port 8000. Other port number could be specified with
'-p' flag.

Command line tool for querying and dumping the database are available.

For usage of those commands, type:

    $ synthdb --help

and brief help messages would be shown.|]

css :: Html
css = toHtml
 "body { font-size: 15px; }\
\ div.wrapper { padding: 20px; }\
\ input { margin: 15px 5px 5px 0; border: 1px solid }\
\ ul { padding-left: 5px; list-style: none; }\
\ a { text-decoration: none; }\
\ div.synthdeffile { margin-top: 15px; }\
\ div.parameters { padding-top: 10px; }\
\ div.ugens { padding-top: 10px; }\
\ h2 { font-size:110%; font-weight: normal; text-decoration:underline; }\
\ code { margin-left: 20px; }\
\ div.stat_summary { padding-top: 15px; }\
\ div.stat_summary_name { float: left; width: 200px; }\
\ div.stat_summary_value { }\
\ div.stat_ugen { float:left; width: 300px; }\
\ div.stat_ugen li { margin-top: 2px;}\
\ div.stat_ugen_name { float: left; width: 150px;}\
\ div.stat_ugen_value { }\
\ div.stat_param { float:left; width: 50%; }\
\ div.stat_param_name { float: left; width: 150px; }\
\ div.stat_param_value { }"
