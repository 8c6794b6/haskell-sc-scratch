{-# LANGUAGE PackageImports #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Helper to build simple GUI from SCNode with GTK.
--
module Sound.SC3.Lepton.GUI
  ( -- * Example
    -- $example

    -- * Types
    Hints
  , ParamRange(..)
    -- * Gui builders
  , treeToGui
  , makeGUIWindow
  ) where

import Control.Monad (forM, replicateM, zipWithM_)
import "mtl" Control.Monad.Trans (liftIO)
import qualified Control.Exception as E
import qualified Data.Map as M

import Data.Generics.Uniplate.Data (universe)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton (SCNode(..), paramToTuple)
import qualified Graphics.UI.Gtk as G

-- $example
--
-- Simple gui for controlling \"foo\" and \"bar\".
--
-- > Module Main where
-- >
-- > import qualified Data.Map as M
-- >
-- > import Sound.SC3
-- > import Sound.SC3.ID
-- > import Sound.SC3.Lepton
-- > import Sound.SC3.Lepton.GUI
-- >
-- > main :: IO ()
-- > main = withsC3 $ \fd -> do
-- >   async fd $ d_recv $ synthdef "foo" foo
-- >   async fd $ d_recv $ synthdef "bar" bar
-- >   addNode 0 tree fd
-- >   treeToGui tree hints fd
-- >
-- > foo :: UGen
-- > foo = out 0 $ sinOsc ar freq 0 * amp
-- >   where
-- >     amp = ctrl "amp" 1
-- >     freq = ctrl "freq" 440
-- >
-- > bar :: UGen
-- > bar = out 0 $ whiteNoise 'a' ar * amp
-- >   where
-- >     amp = ctrl "amp" 1
-- >
-- > tree :: SCNode
-- > tree =
-- >   Group 0
-- >     [Group 1
-- >        [Synth 1000 "foo" ["amp":=0.3,"freq":=440]
-- >        ,Synth 1001 "bar" ["amp":=0.2]]]
-- >
-- > hints :: Hints
-- > hints = M.fromList
-- >    [("foo",[ParamRange "amp" 0 1, ParamRange "freq" 0 440])
-- >    ,("bar",[ParamRange "amp" 0 1])]
-- >
--

-- | Map containing ParamRange for each synthdef.
-- Key is synthdef name, value is list of ParamRange for control parameters.
type Hints = M.Map String [ParamRange]

-- | Data type for range of value for parameter.
data ParamRange = ParamRange
  { prName :: String -- ^ Name of parameter
  , prMin :: Double  -- ^ Min value
  , prMax :: Double  -- ^ Max value
  } deriving (Eq, Show)

-- | Make simple GUI from SCNode.
--
-- Ignoring mapped controls.
--
treeToGui :: (Transport t) => SCNode -> Hints -> t -> IO ()
treeToGui tree hints fd = do
  G.initGUI
  window <- makeGUIWindow tree hints fd
  G.widgetShowAll window
  G.mainGUI

-- | Inner guts of gui builder.
--
-- Makes a window from given SCNode and Hints.
--
makeGUIWindow :: (Transport t)
              => SCNode
              -> Hints
              -> t
              -> IO G.Window
makeGUIWindow tree hints fd = do
  let ps = [(i,n,qs) | Synth i n vs <- universe tree
                     , let qs = concatMap paramToTuple vs]
  window <- G.windowNew
  hPaned <- G.hPanedNew
  buttonBox <- G.vBoxNew True 10
  sliderBox <- G.vBoxNew True 10
  hBoxes <- replicateM (length ps) (G.hBoxNew True 3)
  zipWithM_ (mkSynthControl fd hints buttonBox) hBoxes ps
  G.set sliderBox (map (G.containerChild G.:=) hBoxes)
  G.set hPaned [G.containerChild G.:= buttonBox
               ,G.containerChild G.:= sliderBox]
  G.set window [G.containerBorderWidth G.:= 5
               ,G.containerChild G.:= hPaned]
  window `G.on` G.deleteEvent $ liftIO G.mainQuit >> return False
  return window

-- | Make control for a synth node.
mkSynthControl :: (Transport t)
               => t
               -> Hints                            -- ^ Gui building hints
               -> G.VBox                           -- ^ Box on right side
               -> G.HBox                           -- ^ Box on left side
               -> (Int, String, [(String,Double)]) -- ^ Nid, name, and params
               -> IO G.HBox
mkSynthControl fd hints bb box (i,n,ps) = do
  frame <- G.frameNew
  G.frameSetLabel frame (n ++ ":" ++ show i)
  sliderBox <- G.hBoxNew True 5
  G.set sliderBox [G.boxSpacing G.:= 5, G.containerBorderWidth G.:= 5]
  sliderMaps <- forM ps $ \(n',v) -> do
    let (lo,hi) = rangeFromHints hints n n'
    (sframe,slider) <- mkVSlider i n' v lo hi fd
    G.containerAdd sliderBox sframe
    return (n',slider)
  G.containerAdd frame sliderBox
  G.containerAdd box frame
  mkButtons fd bb i ps sliderMaps
  return box

-- | Make buttons for synth.
mkButtons :: (Transport t)
          => t
          -> G.VBox              -- ^ Container for button box
          -> Int                 -- ^ Node id
          -> [(String,Double)]   -- ^ Params
          -> [(String,G.VScale)] -- ^ Param name and vscales
          -> IO ()
mkButtons fd box i ps vs = do
  pauseButton <- G.toggleButtonNewWithLabel "pause"
  pauseButton `G.on` G.toggled $ do
    st <- G.toggleButtonGetActive pauseButton
    (send fd $ n_run [(i,not st)]) `E.catch` printIOError

  dumpButton <- G.buttonNewWithLabel "dump"
  dumpButton `G.on` G.buttonActivated $ do
    E.handle printIOError $ do
      let o = s_get i $ map fst ps
      m <- send fd o >> wait fd "/n_set"
      print m

  setButton <- G.buttonNewWithLabel "set"
  setButton `G.on` G.buttonActivated $ do
    let f (name,vscl) = do
          d <- G.rangeGetValue vscl
          (send fd $ n_set i [(name,d)]) `E.catch` printIOError
    mapM_ f vs

  getButton <- G.buttonNewWithLabel "get"
  getButton `G.on` G.buttonActivated $ do
    let f (name,vscl) = do
          msg <- send fd (s_get i [name]) >> wait fd "/n_set"
          G.rangeSetValue vscl (extractVal msg)
    mapM_ f vs

  G.set box [G.containerChild G.:= pauseButton
            ,G.containerChild G.:= dumpButton
            ,G.containerChild G.:= setButton
            ,G.containerChild G.:= getButton]

-- | Extracts value from OSC Message.
extractVal :: OSC -> Double
extractVal msg = case msg of
  (Message "/n_set" (_:_:Float v:_)) -> v
  _                                  -> 0

-- | Simple error handler for IOError.
printIOError :: IOError -> IO ()
printIOError = print

-- | Get range for slider.
rangeFromHints :: Hints  -- ^ Hints
               -> String -- ^ Synth name
               -> String -- ^ Param name
               -> (Double,Double)
rangeFromHints hints synName paramName
  | null ps   = defaultValue
  | otherwise = let p = head ps in (prMin p, prMax p)
  where
    ps = filter (\x -> prName x == paramName) $
         maybe [] id (M.lookup synName hints)
    defaultValue = (0, 1)

-- | Add hslider for control param
mkVSlider :: (Transport t)
          => Int    -- ^ Node Id
          -> String -- ^ Param name
          -> Double -- ^ Initial value
          -> Double -- ^ Min value
          -> Double -- ^ Max value
          -> t      -- ^ scsynth connection
          -> IO (G.Frame, G.VScale)
mkVSlider nid name val lo hi fd = do
  v <- G.vScaleNewWithRange lo hi ((hi-lo)/128)
  G.rangeSetValue v val
  frame <- G.frameNew
  G.frameSetLabel frame name
  G.rangeSetInverted v True
  v `G.on` G.valueChanged $ do
    d <- G.rangeGetValue v
    (send fd $ n_set nid [(name,d)]) `E.catch` printIOError
  G.containerAdd frame v
  return (frame, v)
