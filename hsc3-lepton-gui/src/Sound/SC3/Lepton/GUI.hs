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
-- Helper to build simple GUI from SCTree with GTK.
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

import Data.Generics.Uniplate.Data
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton (SCTree(..), paramToTuple)
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
-- >
-- > main :: IO ()
-- > main = withsC3 $ \fd -> do
-- >   async fd $ d_recv $ synthdef "foo" foo
-- >   async fd $ d_recv $ synthdef "bar" bar
-- >   mkTree tree fd
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
-- > tree :: SCTree
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

-- | Make simple GUI from SCTree.
--
-- Ignoring mapped controls.
--
treeToGui :: (Transport t) => SCTree -> Hints -> t -> IO ()
treeToGui tree hints fd = do
  G.initGUI
  window <- makeGUIWindow tree hints fd
  G.widgetShowAll window
  G.mainGUI

-- | Inner guts of gui builder.
--
-- Makes a window from given SCTree and Hints.
--
makeGUIWindow :: (Transport t)
              => SCTree
              -> Hints
              -> t
              -> IO G.Window
makeGUIWindow tree hints fd = do
  let ps = [(i,n,qs) | Synth i n vs <- universe tree
                     , let qs = concatMap paramToTuple vs]
  window <- G.windowNew
  vBox <- G.vBoxNew True 10
  hBoxes <- replicateM (length ps) (G.hBoxNew True 3)
  zipWithM_ (setSliders fd hints) hBoxes ps
  G.set vBox (map (G.containerChild G.:=) hBoxes)
  G.set window [G.containerBorderWidth G.:= 5,G.containerChild G.:= vBox]
  window `G.on` G.deleteEvent $ liftIO G.mainQuit >> return False
  return window

-- | Add sliders to hBox.
setSliders :: (Transport t)
           => t
           -> Hints
           -> G.HBox
           -> (Int, String, [(String,Double)])
           -> IO G.HBox
setSliders fd hints box (i,n,ps) = do
  sliders <- forM ps $ \(n',_) -> do
    let (lo,hi) = rangeFromHints hints n n'
    mkVSlider i n' lo hi fd
  frame <- G.frameNew
  G.frameSetLabel frame (n ++ ":" ++ show i)
  innerBox <- G.hBoxNew True 5
  sliderBox <- G.hBoxNew True 5
  pauseButton <- G.toggleButtonNewWithLabel "pause"
  G.widgetSetSizeRequest pauseButton 20 20 
  pauseButton `G.on` G.toggled $ do
    st <- G.toggleButtonGetActive pauseButton
    (send fd $ n_run [(i,not st)]) `E.catch` printIOError
  G.set sliderBox ([G.boxSpacing G.:= 5
                   ,G.containerBorderWidth G.:= 5])
  --                 ,G.containerChild G.:= pauseButton])
  G.containerAdd sliderBox pauseButton
  mapM_ (G.containerAdd sliderBox) sliders
  G.containerAdd innerBox sliderBox  
  G.containerAdd frame innerBox
  G.containerAdd box frame
  return box
  
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
    defaultValue = (0, 127)

-- | Add hslider for control param
mkVSlider :: (Transport t)
          => Int    -- ^ Node Id
          -> String -- ^ Param name
          -> Double -- ^ Min value
          -> Double -- ^ Max value
          -> t      -- ^ scsynth connection
          -> IO G.Frame
mkVSlider nid name lo hi fd = do
  v <- G.vScaleNewWithRange lo hi ((hi-lo)/128)
  frame <- G.frameNew
  G.frameSetLabel frame name
  G.rangeSetInverted v True
  v `G.on` G.valueChanged $ do
    d <- G.rangeGetValue v
    (send fd $ n_set nid [(name,d)]) `E.catch` printIOError
  G.containerAdd frame v
  return frame
