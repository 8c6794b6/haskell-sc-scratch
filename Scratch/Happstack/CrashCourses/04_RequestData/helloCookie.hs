{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/RqData.html>

-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Happstack.Server
  (CookieLife(..), ServerPart, Response
  ,addCookie, look, mkCookie, nullConf, ok, toResponse
  ,readCookieValue, simpleHTTP, rqPaths, askRq)

main :: IO ()
main = simpleHTTP nullConf homePage

homePage :: ServerPart Response
homePage = msum
  [do rq <- askRq
      liftIO $ print (rqPaths rq)
      mzero
  ,do requests <- readCookieValue "requests"
      addCookie Session $ mkCookie "requests" $ show $ requests+1
      ok $ toResponse $
        "You have mave " ++ show requests ++ " requests to this site."
  ,do addCookie Session $ mkCookie "requests" $ show 2
      ok $ toResponse $ "This is your first request to this site."]
