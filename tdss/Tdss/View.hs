{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Tdss.View where

import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as C8

import Snap.Types (Snap, Request)
import Text.Templating.Heist (Splice)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Network.URI as URI
import qualified Snap.Types as ST
import qualified Text.XmlHtml as X

import Tdss.Model (SearchResult(..))

mkResults :: [SearchResult] -> Splice Snap
mkResults rs = return [X.Element "ul" [] (map mkResult rs)]

mkResult :: SearchResult -> X.Node
mkResult (SearchResult u t) =
    X.Element "li" []
      [X.Element "div" [("id", "result")]
        [X.Element "a" [("href",  E.decodeASCII $ escape u)]
           [X.TextNode $ E.decodeASCII u]
        ,X.Element "div" []
           [X.TextNode $ T.take 200 $ E.decodeASCII t ]]]

escape :: ByteString -> ByteString
escape uri = C8.pack (URI.escapeURIString URI.isAllowedInURI (C8.unpack uri))

mkInputQuery :: Maybe [ByteString] -> Splice Snap
mkInputQuery (Just (q:_)) = return [element]
  where
    element = X.Element "input" [ ("type", "text")
                                , ("name", "q")
                                , ("value", E.decodeASCII q)
                                , ("size", "50") ] []
mkInputQuery _ = return [element]
  where
    element = X.Element "input" [ ("type", "text")
                                , ("name", "q")
                                , ("size", "50") ] []

mkSummary :: [a] -> Request -> Splice Snap
mkSummary ks req = return [element]
  where
    element = if length ks > 0 then
                 X.TextNode $ E.decodeASCII $ C8.concat
                   [num, " hits, page ", cur, " of ", page]
              else
                 X.TextNode $ "no hits"
    num = C8.pack . show . length $ ks
    cur = maybe (C8.pack "1") head $ ST.rqParam (C8.pack "p") req
    page = C8.pack . show $ page'
    page' = case quotRem (length ks) perPage of
              (x,0) -> x
              (x,_) -> x + 1

mkPageLinks :: [a] -> Request -> Splice Snap
mkPageLinks ks req = if length ks > perPage then showIt else dontShowIt
  where
    dontShowIt = return []

    showIt = do
      let q = maybe C8.empty head $ ST.rqParam "q" req
          p = maybe C8.empty head $ ST.rqParam "p" req

          currentPage :: Int
          currentPage = if p' == "" then 1 else read p'
             where p' = C8.unpack p

          mkPageLink num txt =
              X.Element "span"
                [("id", "page_link")]
                [ X.Element "a"
                   [("href", E.decodeASCII $ C8.pack $
                             C8.unpack (ST.rqContextPath req) ++ "?" ++
                             "q=" ++ C8.unpack q ++ "&p=" ++ show num)]
                     [X.TextNode $ E.decodeASCII $ C8.pack (' ' : txt ++ " ")]]

          mkPageLink' n = return $ mkPageLink n (' ':(show n)++" ")

      return [X.Element "div" [("id", "page_links")] $ catMaybes $
                [ if p == "1" || p == C8.empty
                    then Nothing
                    else Just (mkPageLink (currentPage - 1) "<< ") ]
                ++ map mkPageLink'
                       (filter (\x -> x > 0 && x <= lastPage)
                         (enumFromTo (currentPage - (numPageLinks `div` 2))
                                     (currentPage + (numPageLinks `div` 2))))
                ++
                [ if p == (C8.pack $ show lastPage)
                    then Nothing
                    else Just $ mkPageLink (currentPage + 1) " >>" ] ]

    lastPage :: Int
    lastPage = case quotRem (length ks) perPage of
                 (x,0) -> x
                 (x,_) -> x + 1

numPageLinks :: Int
numPageLinks = 20

perPage :: Int
perPage = 10