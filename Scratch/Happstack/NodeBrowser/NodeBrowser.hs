{-# LANGUAGE DeriveDataTypeable #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Web interface to browse synthdef database.

-}
module NodeBrowser where

import Prelude hiding (head,div,span)

import Control.Concurrent
import Control.Monad (forever,msum,when)
import Data.Monoid (mconcat)
import Data.Time.LocalTime

import Control.Monad.Trans (liftIO)
import Happstack.Server hiding (body)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import Text.Blaze.Html5 hiding (base,map,summary)
import Text.Blaze.Html5.Attributes hiding
  (dir,id,title,form,style,span,summary)

import qualified Text.Blaze.Html5.Attributes as A

instance ToHtml SCNode where
  toHtml (Group gid ns) = do
    div ! 
      class_ (toValue "group_node") ! 
      A.id (toValue $ "node_" ++ show gid) $ do
        div ! class_ (toValue "group_id") $ do
          span ! class_ (toValue "group_id_num") $ toHtml $ show gid
          span ! class_ (toValue "group_id_symbol") $ toHtml " group"
          when (gid /= 0) $ do 
            span ! class_ (toValue "synth_free") $ do
              button ! onclick (toValue $ "n_free(" ++ show gid ++ ")") $ 
                toHtml "free"
        div ! class_ (toValue "group_children") $ do
          mapM_ toHtml ns
  toHtml (Synth nid defn ps) = do
    div 
      ! class_ (toValue "synth_node") 
      ! A.id (toValue $ "node_" ++ show nid) $ do
      div ! class_ (toValue "synth_id") $ do
        span ! class_ (toValue "synth_id_num") $ toHtml $ show nid
        span ! class_ (toValue "synth_id_name") $ do
          -- XXX: get this link from command line arg.
          a ! href (toValue $ "http://localhost:8002/synthdef/" ++ defn) $ do
            toHtml defn
        span ! class_ (toValue "synth_free") $ do
          button ! onclick (toValue $ "n_free(" ++ show nid ++ ")") $ 
            toHtml "free"
      div ! class_ (toValue "synth_params") $ do
        mapM_ toHtml ps

instance ToHtml SynthParam where
  toHtml (n:=v) = do
    span ! class_ (toValue "param_value") $ do
      span ! class_ (toValue "param_name") $ toHtml (n ++ ":")
      span !
        class_ (toValue "param_value_num") $ do
          input ! 
            type_ (toValue "text") !
            A.name (toValue n) !
            value (toValue $ show v) !
            size (toValue "5") !
            onchange (toValue "n_set(this)")
  toHtml (n:<-v) = do
    span ! class_ (toValue "param_control_bus") $ do
      span ! class_ (toValue "param_name") $ toHtml (n ++ ":")
      span ! class_ (toValue "param_control_bus_num") $ toHtml ('c':show v)
  toHtml (n:<=v) = do
    span ! class_ (toValue "param_audio_bus") $ do
      span ! class_ (toValue "param_name") $ toHtml (n ++ ":")
      span ! class_ (toValue "param_audio_bus_num") $ toHtml ('a':show v)

main :: IO ()
main = withSC3 $ \fd -> do
  putStrLn $ "Starting node browser."
  send fd $ notify True
  wait fd "/done"
  mv <- newMVar =<< getRootNode fd
  tid <- forkIO $ nodeWatcher mv fd
  serve mv

nodeWatcher :: Transport t => MVar SCNode -> t -> IO ()
nodeWatcher mv fd = do
  putStrLn "Starting node watching loop."
  forever $ do
    msg <- recv fd
    now <- getZonedTime
    putStrLn $ show now ++ " " ++ show msg
    tree <- getRootNode fd
    modifyMVar_ mv (const $ getRootNode fd)

serve :: MVar SCNode -> IO ()
serve mv = simpleHTTP nullConf $ msum
  [ dir "favicon.ico" $ notFound $ toResponse ()
  , dir "n_set" $ path $ \nid -> nSet mv (read nid)
  , dir "n_free" $ path $ \nid -> nFree mv (read nid)
  , dir "echo" echo
  , dir "js" $ serveDirectory EnableBrowsing [] "js"    
  , dir "css" $ serveDirectory EnableBrowsing [] "css"
  , showTree mv  
  ]

showTree :: MVar SCNode -> ServerPartT IO Response
showTree mv = do
  tree <- liftIO $ readMVar mv
  ok $ toResponse $ do 
    preEscapedString "<!doctype html>" 
    html $ do
      head $ do
        title $ toHtml "showTree"
        meta !
          httpEquiv (toValue "Content-type") !
          content (toValue "text/html;charset=UTF-8")
        css
        js
      body $ do
        div ! class_ (toValue "root") $ toHtml tree

nSet :: MVar SCNode -> Int -> ServerPartT IO Response
nSet mv nid = do
  rq <- askRq
  let paramStrings = mkParam $ rqQuery rq
      paramValues = map (\(a,b) -> (a,read b)) paramStrings
      msg = n_set nid paramValues
  liftIO $ withSC3 $ \fd -> do
    send fd msg
    modifyMVar_ mv (const $ getRootNode fd)
  ok $ toResponse $ show msg

echo :: ServerPartT IO Response
echo = do
  rq <- askRq
  liftIO $ print rq
  ok $ toResponse $ show rq

mkParam :: String -> [(String,String)]
mkParam qs | null qs = []
           | otherwise = go '&' [] (tail qs)
  where
    go c acc [] = acc
    go c acc xs | null ys = y:acc | otherwise = y:go c acc (tail ys)
      where (y',ys) = break (== c) xs
            y | null zs = (z,[]) | otherwise = (z,tail zs)
            (z,zs) = break (== '=') y'

nFree :: MVar SCNode -> Int -> ServerPartT IO Response
nFree mv nid = do
  liftIO $ withSC3 $ \fd -> do
    send fd $ n_free [nid]
    modifyMVar_ mv (const $ getRootNode fd)
  showTree mv

css :: Html
css = 
  link !
  rel (toValue "stylesheet") ! 
  type_ (toValue "text/css") ! 
  href (toValue "css/style.css")

js :: Html
js = do
  script !
    type_ (toValue "text/javascript") !
    src (toValue "js/jquery-1.6.2.min.js") $ return ()
  script !
    type_ (toValue "text/javascript") !
    src (toValue "js/nb.js") $ return ()
