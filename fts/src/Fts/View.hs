{-# LANGUAGE OverloadedStrings,
             PackageImports
  #-}
module Fts.View where

import Data.ByteString ( ByteString )
import Data.Maybe ( catMaybes )
import Snap.Types ( Snap, Request ) 
import Text.Templating.Heist ( Splice, Node )

import "monads-fd" Control.Monad.Trans ( liftIO )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Network.URI as URI
import qualified Snap.Types as ST
import qualified Text.Templating.Heist as HE
import qualified Text.XML.Expat.Tree as EX

import Fts.Model ( SearchResult(..) )

mkResults :: [SearchResult] -> Splice Snap
mkResults rs = return [EX.mkElement "ul" [] (map mkResult rs)]

mkResult :: SearchResult -> Node
mkResult (SearchResult u t) = 
    EX.mkElement "li" []
      [ EX.mkElement "div" [("id", "result")] 
        [ EX.mkElement "a" [("href", escape u)] [EX.mkText u]
        , EX.mkElement "div" [] 
                           [EX.mkText $ C8.append (C8.take 200 t) " ..."] ]]

escape :: ByteString -> ByteString
escape uri = C8.pack (URI.escapeURIString URI.isAllowedInURI (C8.unpack uri))

mkSummary :: [a] -> Request -> Splice Snap
mkSummary ks req = do
  return [elem]
  where
    elem = EX.mkText $ foldr C8.append C8.empty
           [num, " hits, page ", cur, " of ", page]
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

          mkPageLink :: Int -> String -> Node
          mkPageLink num txt = 
              EX.mkElement "span"
                [("id", "page_link")] 
                [ EX.mkElement "a" 
                   [("href", C8.pack $ 
                             C8.unpack (ST.rqContextPath req) ++ "?" ++ 
                             "q=" ++ C8.unpack q ++ "&p=" ++ show num)]
                     [EX.mkText $ C8.pack (' ' : txt ++ " ")]]

          mkPageLink' :: Int -> Maybe Node
          mkPageLink' n = return $ mkPageLink n (' ':(show n)++" ")

      return [EX.mkElement "div" [("id", "page_links")] $ catMaybes $ 
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