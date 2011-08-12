{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/Templates.html>

-}
module Main where

import Happstack.Server
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = simpleHTTP nullConf helloBlaze

helloBlaze :: ServerPart Response
helloBlaze = 
  ok $ toResponse $
  appTemplate "Hello, Blaze!"
  [H.meta ! A.name "keywords" ! A.content "happstack, blaze, html"]
  (H.p "hello, blaze!")
  
appTemplate :: String -> [H.Html] -> H.Html -> H.Html  
appTemplate title headers body = 
  H.html $ do
    H.head $ do
      H.title (H.toHtml title)
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      sequence_ headers
    H.body $ do
      body