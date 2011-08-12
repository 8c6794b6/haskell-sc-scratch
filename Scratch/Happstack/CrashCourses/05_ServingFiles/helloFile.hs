{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/FileServing.html>

-}
module Main where

import Control.Monad (msum)

import Happstack.Server
  (ServerPart, Response, Browsing(..), nullConf, serveDirectory, serveFile, dir
  ,simpleHTTP, asContentType, ok, toResponse)
import Text.Blaze.Html5 
import Text.Blaze.Html5.Attributes hiding (dir, title)

import Prelude hiding (id, head, div)

main :: IO ()  
main = simpleHTTP nullConf $ msum 
 [ dir "static" $ 
     serveDirectory EnableBrowsing [] "/home/atsuro/public_html"
 , dir "helloFile.hs" $ 
     serveFile (asContentType "text/x-haskell") "helloFile.hs"
 , links ]
 
links :: ServerPart Response
links = ok $ toResponse $ contents

contents :: Html
contents = html $ do
  head $ do
    title $ "Serving files"
  body $ do
    h1 $ "Serving files"
    ul $ do
      li $ a !
        href "/static" $ "static files under public_html"
      li $ a ! 
        href "/helloFile.hs" $ "this source code"
