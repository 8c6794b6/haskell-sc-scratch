{-# LANGUAGE OverloadedStrings #-}
module Fts.View where

import Data.ByteString ( ByteString )
import Snap.Types ( Snap, Request ) 
import Text.Templating.Heist ( Splice, Node )

import qualified Data.ByteString.Char8 as C8
import qualified Network.URI as URI
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
        , EX.mkElement "div" [] [EX.mkText $ C8.take 200 t]]]
  where
    escape uri = C8.pack (URI.escapeURIString URI.isAllowedInURI (C8.unpack uri))

pageLinks :: Request -> Splice Snap
pageLinks = undefined