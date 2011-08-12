{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/Templates.html>

Run the app and view <http://localhost:8000/factorial/> .

-}
module Main where

import Control.Monad (msum)

import Control.Monad.Trans (MonadIO)
import Happstack.Server (dir, nullConf, nullDir, simpleHTTP)
import Happstack.Server.Heist (templateServe, templateReloader)
import Text.Templating.Heist 
  (TemplateMonad, Template, TemplateState
  ,bindSplice, emptyTemplateState, getParamNode)
import Text.Templating.Heist.TemplateDirectory (newTemplateDirectory')  

import qualified Data.Text as T
import qualified Text.XmlHtml as X

main :: IO ()
main = do
  let templateDir = "."
  td <- newTemplateDirectory' templateDir (templateState templateDir)
  simpleHTTP nullConf $ msum
    [ templateServe td
    , dir "reload" $ nullDir >> templateReloader td ]
    
templateState :: MonadIO m => FilePath -> TemplateState m
templateState td = bindSplice (T.pack "fact") factSplice (emptyTemplateState td)

factSplice :: Monad m => TemplateMonad m Template
factSplice = do
  input <- getParamNode
  let text = T.unpack $ X.nodeText input
      n = read text :: Int
  return [X.TextNode $ T.pack $ show $ product [1..n]]
