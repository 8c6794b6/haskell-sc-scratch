{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/RqData.html>

-}
module Main where

import Happstack.Server (ServerPart, look, nullConf, simpleHTTP, ok)

main :: IO ()
main = simpleHTTP nullConf helloPart

helloPart :: ServerPart String
helloPart = do
  greeting <- look "greeting"
  noun <- look "noun"
  ok $ greeting ++ ", " ++ noun
