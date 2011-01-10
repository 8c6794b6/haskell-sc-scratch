------------------------------------------------------------------------------
-- | Simple gtk gui controlling fm synth in supercollider.
-- 

module HelloGTK where

import Control.Applicative
import Graphics.UI.Gtk
import Sound.OpenSoundControl
import Sound.SC3

main :: IO ()
main = do
  initGUI
  window <- windowNew
  widget' <- widget
  set window [containerBorderWidth := 10, 
              containerChild := widget']
  window `onDestroy` mainQuit
  widgetShowAll window
  mainGUI

widget :: IO HBox
widget = do
  hbox <- hBoxNew True 10
  set hbox =<< (sequence $ fmap (fmap (containerChild :=)) 
                [mkScale (setNodeValue "cfreq") 10 1000,
                 mkScale (setNodeValue "cidx") 10 1000,
                 mkScale (setNodeValue "coffset") 10 1000])
  return hbox

mkScale :: (Double -> IO ()) -> Double -> Double -> IO VScale
mkScale act min max = do
  scale <- vScaleNewWithRange 10 1000 1
  rangeSetInverted scale True
  onRangeValueChanged scale $ get scale rangeValue >>= act
  return scale

setNodeValue :: String -> Double -> IO ()
setNodeValue name val = 
    withSC3 $ \fd -> send fd $ n_set pitchedNodeId [(name,val)]

pitchedNodeId :: Int
pitchedNodeId = 1000

simpleFMSynth :: UGen
simpleFMSynth = out 0 $ mce [sig, sig]
    where
      sig = sinOsc ar freq 0 * 0.3
      freq = sinOsc ar cfreq 0 * cidx + coffset
      cfreq = control kr "cfreq" 440 
      cidx = control kr "cidx" 440
      coffset = control kr "coffset" 0

setSC :: Transport t => t -> IO ()
setSC fd = do
    async fd $ d_recv $ synthdef "simpleFMSynth" simpleFMSynth
    send fd $ s_new "simpleFMSynth" pitchedNodeId AddToTail 1 []
