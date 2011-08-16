{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Web interface to browse synth nodes in running scsynth server.

/TODO/:

* Add js hot key support for:

    * Moving focus to right area and left area.

    * Showing help of which key does what.

* Add "dump current setting" feature.

* Add feature for adding new nodes.

   * Enable adding group node with specified add action.

   * Enable adding synth node with specified add action.

* Add ui for setting control bus, c_set message.

* Add ui for buffer

    * Use buffer, what kind of operation would be convinient?.

-}
module NodeBrowser where

import Prelude hiding (head,div,span)

import Control.Concurrent
import Control.Monad (forever,msum,when)
import Data.Monoid (mconcat)
import Data.Time.LocalTime

import Control.Monad.Trans (liftIO)
import Data.Generics.Uniplate.Operations
import Happstack.Server hiding (body)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import Text.Blaze.Html5 hiding (base,map,summary)
import Text.Blaze.Html5.Attributes hiding (dir,id,title,form,style,span,summary)

import qualified Text.Blaze.Html5.Attributes as A
import qualified Prelude

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
  , dir "n_map" $ path $ \nid -> nMap mv (read nid)
  , dir "n_mapa" $ path $ \nid -> nMapa mv (read nid)
  , dir "n_free" $ path $ \nid -> nFree mv (read nid)
  , dir "nodeDetail" $ path $ \nid -> nodeDetail mv (read nid)
  , dir "echo" echo
  , dir "js" $ serveDirectory EnableBrowsing [] "js"
  , dir "css" $ serveDirectory EnableBrowsing [] "css"
  , showTree mv
  ]

showTree :: MVar SCNode -> ServerPartT IO Response
showTree mv = do
  tree <- liftIO $ readMVar mv
  statReply <- liftIO $ withSC3 $ \fd -> do
    send fd status
    wait fd "/status.reply"
  ok $ toResponse $ do
    preEscapedString "<!doctype html>"
    html $ do
      head $ do
        title $ "showTree"
        meta ! httpEquiv "Content-type" ! content "text/html;charset=UTF-8"
        css
        js
      body $ do
        div ! class_ "wrapper" $ do
          div ! class_ "root" $ toHtml tree
          div ! class_ "right" $ rightPart statReply
        script ! type_ "text/javascript" $ "init()"

rightPart :: OSC -> Html
rightPart statMsg = do
  div ! class_ "right_wrapper" $ do
    div ! class_ "right_header" $ showStat statMsg
    div ! class_ "right_main" $ do
      div $ helpMessage
    div ! class_ "right_footer" $ return ()

showStat :: OSC -> Html
showStat msg = case msg of
  Message "/status.reply"
    (_:Int nU:Int nS:Int nG:Int nI:Float cA:Float cP:Double sN:Double sA:_) ->
      div ! class_ "synth_stat" $ do
        ul ! class_ "synth_stat_elems" $ do
          li $ do
            span ! class_ "synth_stat_name" $ "ugens: "
            toHtml $ show nU ++ ", "
            span ! class_ "synth_stat_name" $ " synths: "
            toHtml $ show nS ++ ", "
            span ! class_ "synth_stat_name" $ " groups: "
            toHtml $ show nG ++ ", "
            span ! class_ "synth_stat_name" $ " instr: "
            toHtml $ show nI
          li $ do
            span ! class_ "synth_stat_name" $ "cpu : "
            toHtml $ show cA ++ " / " ++ show cP
          li $ do
            span ! class_ "synth_stat_name" $ "sample rate : "
            toHtml $ show sN ++ " / " ++ show sA

withParamValues :: Read t1
                => (t -> [(String,t1)] -> OSC) -> (String -> String)
                -> MVar SCNode -> t -> ServerPartT IO Response
withParamValues f g mv nid = do
  rq <- askRq
  let paramStrings = mkParam $ rqQuery rq
      paramValues = map (\(a,b) -> (a,read $ g b)) paramStrings
      msg = f nid paramValues
  liftIO $ withSC3 $ \fd -> do
    send fd msg
    print msg
    modifyMVar_ mv (const $ getRootNode fd)
  ok $ toResponse $ show msg

nSet :: MVar SCNode -> Int -> ServerPartT IO Response
nSet = withParamValues n_set id

nMap :: MVar SCNode -> Int -> ServerPartT IO Response
nMap = withParamValues n_map tail

nMapa :: MVar SCNode -> Int -> ServerPartT IO Response
nMapa = withParamValues n_mapa tail

echo :: ServerPartT IO Response
echo = do
  rq <- askRq
  liftIO $ print rq
  ok $ toResponse $ show rq

nFree :: MVar SCNode -> Int -> ServerPartT IO Response
nFree mv nid = do
  liftIO $ withSC3 $ \fd -> do
    send fd $ n_free [nid]
    modifyMVar_ mv (const $ getRootNode fd)
  showTree mv

nodeDetail :: MVar SCNode -> Int -> ServerPartT IO Response
nodeDetail mv nid = do
  t <- liftIO $ readMVar mv
  let node = Prelude.head $ universeBi [n|n <- universeBi t, nodeId n == nid]
  ok $ toResponse $ do
    div $ synthNodeHtml node True

mkParam :: String -> [(String,String)]
mkParam qs | null qs = []
           | otherwise = go '&' [] (tail qs)
  where
    go c acc [] = acc
    go c acc xs | null ys = y:acc | otherwise = y:go c acc (tail ys)
      where (y',ys) = break (== c) xs
            y | null zs = (z,[]) | otherwise = (z,tail zs)
            (z,zs) = break (== '=') y'

instance ToHtml SCNode where
  toHtml (Group gid ns) = do
    div !
      class_ "group_node" !
      A.id (toValue $ "node_" ++ show gid) $ do
        div ! class_ "group_id" $ do
          span ! class_ "group_id_num" $ toHtml $ show gid
          span ! class_ "group_id_symbol" $ " group"
          when (gid /= 0) $ do
            span ! class_ "synth_free" $ do
              button ! onclick (toValue $ "n_free(" ++ show gid ++ ")") $
                "free"
        div ! class_ "group_children" $ do
          mapM_ toHtml ns
  toHtml n@(Synth nid defn ps) = synthNodeHtml n False

synthNodeHtml :: SCNode -> Bool -> Html
synthNodeHtml (Group _ _) _ = return ()
synthNodeHtml (Synth nid defn ps) detailed = do
  let mainDiv = mkSynthMain nid detailed
  mainDiv $ do
    div ! class_ "synth_id" $ do
      span ! class_ "synth_id_num" $ toHtml $ show nid
      span ! class_ "synth_id_name" $ do
        -- XXX: get this link from command line arg.
        a ! href (toValue $ "http://localhost:8002/synthdef/" ++ defn) $ do
          toHtml defn
      span ! class_ ("synth_free") $ do
        button ! onclick (toValue $ "n_free(" ++ show nid ++ ")") $
          "free"
    when detailed $
      div ! class_ ("synth_params") $ do
        mapM_ toHtml ps

mkSynthMain :: Int -> Bool -> Html -> Html
mkSynthMain nid detailed h
  | detailed =
    div ! class_ "synth_node_detail" !
    A.id (toValue $ "node_" ++ show nid) $ h
  | otherwise =
    div ! class_ "synth_node" !
    A.id (toValue $ "node_" ++ show nid ++ "_tree") !
    onclick (toValue $ "node_detail(" ++ show nid ++ ")") $ h

instance ToHtml SynthParam where
  toHtml (n:=v)  = mkParamHtml n (show v)
  toHtml (n:<-v) = mkParamHtml n ('c':show v)
  toHtml (n:<=v) = mkParamHtml n ('a':show v)

mkParamHtml :: String -> String -> Html
mkParamHtml n val = do
  div ! class_ "synth_param" $ do
    div ! class_ "param_value" $ do
      div ! class_ "param_name" $ toHtml (n ++ ":")
      span ! class_ "param_value_num" $ do
        input !
          type_ "text" !
          A.name (toValue n) !
          value (toValue val) !
          size "15" !
          onchange "param_changed(this)"

css :: Html
css =
  link !
  rel "stylesheet" !
  type_ "text/css" !
  href "css/style.css"

js :: Html
js = do
  script !
    type_ "text/javascript" !
    src "js/jquery-1.6.2.min.js" $ return ()
  script !
    type_ "text/javascript" !
    src "js/jquery.hotkeys.js" $ return ()
  script !
    type_ "text/javascript" !
    src "js/nb.js" $ return ()

helpMessage :: Html
helpMessage = do
  p "node browser"
  p "key bindings: "
  ul $ do
    li $ "j - move focused node down"
    li $ "k - move focused node up"
    li $ "i - focus first input in right area"
