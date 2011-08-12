{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/RouteFilters.html>

-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Char (toLower)
-- import Happstack.Server (nullConf, simpleHTTP, ok, dir, path)
import Happstack.Server

main :: IO ()
main =
  simpleHTTP nullConf $ msum
  [ methodM [GET, HEAD] >> ok "You did a GET request.\n"
  , methodM POST >> ok "You did a POST request.\n"
  , methodM [GET, HEAD] >> (dir "method" $ ok "Hello, method\n")
    
  , dir "hello" $ dir "world" $ ok "Hello World!"
    
    -- Shorthand match on multiple components are possible 
    -- ... not working?
    -- , dir "goodbye/moon" $ ok "Goodbye, moon!"
    
    -- Using path action.
  , dir "greet" $ path $ \s -> ok $ "Hello, " ++ s
                               
    -- Using own type and feed path function
  , dir "subject" $ path $ ok . sayHello
  
    -- FilterMonad has MonadIO instance, we can do almost anything here.
  , dir "print" $ path $ \s -> liftIO (putStrLn s) >> ok ("Got " ++ s)]

data Subject = World | Haskell

sayHello :: Subject -> String
sayHello World   = "Hello, World!"
sayHello Haskell = "Greeting, Haskell!"

instance FromReqURI Subject where
  fromReqURI sub = case map toLower sub of
    "haskell" -> Just Haskell
    "world"   -> Just World
    _         -> Nothing
