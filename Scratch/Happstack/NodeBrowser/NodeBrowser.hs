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
import Control.Monad (forever,msum)
import Data.Time.LocalTime

import Control.Monad.Trans (liftIO)
import Happstack.Server hiding (body)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import Text.Blaze.Html5 hiding (base,map,summary)
import Text.Blaze.Html5.Attributes hiding 
  (dir,id,title,form,style,span,size,summary)
  
instance ToHtml SCNode where  
  toHtml (Group gid ns) = do
    div ! class_ (toValue "group_node") $ do
      div ! class_ (toValue "group_id") $ do 
        span ! class_ (toValue "synth_free") $ do
          a ! href (toValue $ "/n_free/" ++ show gid) $ toHtml " x "
        span ! class_ (toValue "group_id_num") $ toHtml $ show gid 
        span ! class_ (toValue "group_id_symbol") $ toHtml " group"
      div ! class_ (toValue "group_children") $ do 
        mapM_ toHtml ns
        
  toHtml (Synth nid defn ps) = do
    div ! class_ (toValue "synth_node") $ do
      div ! class_ (toValue "synth_id") $ do 
        span ! class_ (toValue "synth_free") $ do
          a ! href (toValue $ "/n_free/" ++ show nid) $ toHtml " x "
        span ! class_ (toValue "synth_id_num") $ toHtml $ show nid
        span ! class_ (toValue "synth_id_name") $ do 
          -- XXX: Where to get this link?
          a ! href (toValue $ "http://localhost:8002/synthdef/" ++ defn) $ do
            toHtml defn
      div ! class_ (toValue "synth_params") $ do
        mapM_ toHtml ps

instance ToHtml SynthParam where
  toHtml (n:=v) = do 
    span ! class_ (toValue "param_value") $ do
      span ! class_ (toValue "param_name") $ toHtml (n ++ ":")
      span ! class_ (toValue "param_value_num") $ toHtml $ show v
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
  , dir "n_set" $ path $ \nid -> nSet (read nid)
  , dir "n_free" $ path $ \nid -> nFree mv (read nid)
  , showTree mv ]
  
showTree :: MVar SCNode -> ServerPartT IO Response  
showTree mv = do 
  tree <- liftIO $ readMVar mv
  ok $ toResponse $ html $ do
    head $ do
      title $ toHtml "showTree"
      css 
    body $ do
      div $ toHtml tree

nSet :: Int -> ServerPartT IO Response
nSet nid = do
  rq <- askRq
  ok $ toResponse $ html $ do
    div $ toHtml $ unwords ["n_set", show nid] 
    div $ toHtml $ "rqQuery = " ++ rqQuery rq
    div $ toHtml $ "rqInputsQuery = " ++ show (rqInputsQuery rq)
    
nFree :: MVar SCNode -> Int -> ServerPartT IO Response    
nFree mv nid = do
  liftIO $ withSC3 $ \fd -> do 
    send fd $ n_free [nid]
    modifyMVar_ mv (const $ getRootNode fd)
  showTree mv
      
css :: Html      
css = style ! type_ (toValue "text/css") $ toHtml
 "body { font-size: 14px; } \
\a { text-decoration: none; color: #3344FF;} \
\a:visited { color: #3344FF; } \
\a:hover { text-decoration: underline; } \
\div.group_node { margin-left: 20px; padding: 3px; } \
\div.group_id { } \
\span.group_id_num { color: #FA0300; } \
\span.group_id_symbol { } \
\div.group_children { } \
\div.synth_node { margin-left: 20px; padding: 2px; } \
\div.synth_id { } \
\span.synth_id_num { color: #129A13; } \
\span.synth_id_name { padding-left: 5px; } \
\div.synth_params { margin-left: 20px; border-left: solid 2px #9A9A9A; } \
\span.param_value { padding-left: 10px; } \
\span.param_control_bus { padding-left: 10px; } \
\span.param_audio_bus { padding-left: 10px; }"
