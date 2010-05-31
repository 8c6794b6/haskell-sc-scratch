------------------------------------------------------------------------------
-- |
--
-- Module to control synth node. Node would be specifyed by node id.
--
-- XXX: Rewrite using ReaderT IO (cause nodeId etc are used everywhere)..
--

module Scratch.GUI.NodeControl where

import Data.Map (Map)
import qualified Data.Map as M

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk as G

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Wing
import qualified Sound.SC3.Wing.Tree as T
import qualified Sound.SC3.Wing.Query as Q

import Scratch.CLI.NodeControl
    ( getNodeById,
      getParams )

main :: IO ()
main = nodeController 1001 $ simpleGendy1PR

simpleGendy1PR :: ParamRange
simpleGendy1PR =
    M.fromList [("ampDist", (0,1)),
                ("durDist", (0,1)),
                ("adParam", (0,1)),
                ("ddParam", (0,1)),
                ("minFreq", (0,440)),
                ("maxFreq", (0,880)),
                ("ampScale", (0,1)),
                ("durScale", (0,1)),
                ("initCPS", (1,24)),
                ("kNum", (1,24)),
                ("pos", (-1,1))]


-- | Type synonym for specifying range of control arg in GUI.
type ParamRange = Map String (Double, Double)

-- | Makes a GUI controller from synth node.
nodeController :: NodeId -> ParamRange -> IO ()
nodeController nid ranges = do
  node <- fmap (getNodeById nid . parseOSC) (withSC3 queryTree)
  maybe failed (mkGUI ranges) node
      where
        failed = error "GUI creation failed, wrong ID?"

-- | Inner guts of nodeController.
mkGUI :: ParamRange -> SCTree -> IO ()
mkGUI ranges syn@(Synth nid name ps) = do
  initGUI
  window <- windowNew
  widget <- mkSliders nid pNames ranges
  set window [containerBorderWidth G.:= 10,
              containerChild G.:= widget]
  window `onDestroy` mainQuit
  widgetShowAll window
  mainGUI
  where
    pNames = map pName ps
    pName (n T.:= _)  = n
    pName (n T.:<- _) = n

mkSliders :: NodeId -> [String] -> ParamRange -> IO HBox
mkSliders nid names ranges = do
  hbox <- hBoxNew True 10
  vs <- mapM (mkSlider nid ranges) names
  set hbox $ map (containerChild G.:=) vs
  return hbox

mkSlider :: NodeId -> ParamRange -> String -> IO VScale
mkSlider nid range name = do
  let (min,max) = maybe (0,1) id $ M.lookup name range
  scale <- vScaleNewWithRange min max 0.01
  rangeSetInverted scale True
  onRangeValueChanged scale $ get scale rangeValue >>= setNodeValue nid name
  return scale

setNodeValue :: NodeId -> String -> (Double -> IO ())
setNodeValue nid name value = do
  withSC3 $ \fd -> send fd $ n_set nid [(name,value)]
