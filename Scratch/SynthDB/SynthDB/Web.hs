{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
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

import Prelude hiding ((.),id,div,head)

import Control.Category (Category(..))
import Control.Monad (msum,zipWithM_)
import Data.Monoid (mconcat)

import Control.Monad.Trans (liftIO)
import Happstack.Server hiding (body, method)
import Sound.SC3
import Sound.SC3.Lepton.Parser
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir,id,title,form,style)
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes
import Web.Routes.Happstack (implSite)
import Web.Routes.Boomerang

import qualified Data.ByteString.Char8 as C8

import SynthDB.Persist

data Sitemap
  = SList
  | SDetail String

$(derivePrinterParsers ''Sitemap)
rSList :: PrinterParser e tok r (Sitemap :- r)
rSDetail :: PrinterParser e tok (String :- r) (Sitemap :- r)

serve :: IO ()
serve = simpleHTTP nullConf $ msum
  [ dir "favicon.ico" $ notFound (toResponse ())
  , implSite "http://localhost:8000/" "" site
  , seeOther "" (toResponse ())
  ]

site :: Site Sitemap (ServerPartT IO Response)
site = setDefault SList $ boomerangSite (runRouteT route) urlmap

urlmap :: Router Sitemap
urlmap =
  rSList <>
  rSDetail . (lit "detail" </> rList anyChar)

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
  SList           -> listPage
  SDetail defName -> detailPage defName

listPage :: RouteT Sitemap (ServerPartT IO) Response
listPage = do
  qs <- getDataFn $ look "q"
  listURL <- showURL SList
  case qs of
    Left _   -> do
      ok $ toResponse $ html $ do
        head $ do
          title $ toHtml "search"
          style ! type_ (toValue "text/css") $ css
        body $ do
          div ! class_ (toValue "wrapper") $ do
            div ! class_ (toValue "searchform") $ inputForm listURL ""
    Right qs' -> do
      rs <- liftIO $ queryUGs qs'
      links <- mapM mkLink rs
      ok $ toResponse $ html $ do
        head $ do
          title $ toHtml $ "search result for: " ++ qs'
          style ! type_ (toValue "text/css") $ css
        body $ do
          div ! class_ (toValue "wrapper") $ do
            div ! class_ (toValue "searchform") $ do
              inputForm listURL qs'
            div $ ul $ mconcat links
  where
    inputForm actionURL lastVal = do
      form ! enctype (toValue "multipart/form-data") !
        method (toValue "GET") ! action (toValue actionURL) $ do
          input ! type_ (toValue "text") ! name (toValue "q") !
            value (toValue lastVal)
          input ! type_ (toValue "submit") ! value (toValue "search")
    mkLink sd = do
      let sdn = f (sdrName sd)
          f (SDName n) = C8.unpack n
      url <- showURL (SDetail sdn)
      return $ li $ a ! href (toValue url) $ toHtml sdn

detailPage :: String -> RouteT Sitemap (ServerPartT IO) Response
detailPage defName = do
  sd <- liftIO $ getDef defName
  ok $ toResponse $ html $ do
    head $ do
      title $ toHtml $ "synthdef: " ++ (unwords $ map sdName $ sdDefs sd)
      style ! type_ (toValue "text/css") $ css
    body $ do
      div ! class_ (toValue "wrapper") $ do
        toHtml sd

css :: Html
css = toHtml
 "body { font-size: 15px; }\
\ div.wrapper { padding: 20px; }\
\ input { margin: 5px; border: 1px solid }\
\ ul { list-style: none; }\
\ div.parameters { padding-top: 10px; }\
\ div.ugens { padding-top: 10px; }"

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
          show idx ++ ": " ++ uName un sp ++ " " ++ showRate r ++ " " ++
          unwords (map iSpec is) ++ " " ++
          unwords (map oSpec os)

      uName un sp | un == "BinaryOpUGen" = binaryName $ fromIntegral sp
                  | un == "UnaryOpUGen"  = unaryName $ fromIntegral sp
                  | otherwise            = un

      iSpec idx = case idx of
        IsConstant cix   ->
          "(" ++ show (cs !! fromIntegral cix) ++ ")"
        IsUGenOut ugi oix ->
          "(ugen " ++ show (fromIntegral ugi::Int) ++ ":" ++ show oix ++ ")"

      oSpec o = "(out:" ++ showRate o ++ ")"

      showRate r = case r of
        0 -> "ir"; 1 -> "kr"; 2 -> "ar"; 3 -> "dr"; _ -> ""
