{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/HelloWorld.html>

-}

module Main where

import Happstack.Server

main :: IO ()
main = simpleHTTP nullConf $ ok $ toResponse "Hello, World!"