------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook03
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook03_External_Control/.
--

module SCHelp.PG.Cookbook03 (
    main,

    -- * Control of parameters by MIDI or HID
    -- $controlByHID
    runByHID,
    playByKey,
    playHIDNote,

    -- * Triggering a pattern by a GUI
    -- $triggeringByGUI
    runByGUI,
    guiContents,
    guiContainer,

    -- * Triggering a pattern by signal amplitude
    -- $triggeringBySignalAmplitude
    runBySignalAmplitude

    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.IO

import Graphics.UI.Gtk
import Sound.OpenSoundControl
import Sound.SC3

import Reusable
import SCQuery
import SCTree hiding ((:=))
import SCSched

main :: IO ()
main = runByGUI

-- $controlByHID
--
-- Translation of @Phidkey@ and @PhidSlot@ pattern in sclang.
-- Triggering sending of message with key press. 'a' is midiNote 60,
-- and 'w' is 61, as so on until 'k', 72.
--
runByHID :: IO ()
runByHID = ini >> forever g 
    where 
      g = getChar >>= playByKey playHIDNote
      ini = hSetBuffering stdin NoBuffering >> hSetEcho stdin False

playByKey :: (Double -> IO ()) -> Char -> IO ()
playByKey f 'a' = f 60
playByKey f 'w' = f 61
playByKey f 's' = f 62
playByKey f 'e' = f 63
playByKey f 'd' = f 64
playByKey f 'f' = f 65
playByKey f 't' = f 66
playByKey f 'g' = f 67
playByKey f 'y' = f 68
playByKey f 'h' = f 69
playByKey f 'u' = f 70
playByKey f 'j' = f 71
playByKey f 'k' = f 72
playByKey f 'q' = return ()
playByKey _ _ = return ()

playHIDNote :: Double -> IO ()
playHIDNote note = withSC3 $ \fd -> do
   send fd (s_new "simpleSynth" (-1) AddToTail 1
            [("dur",1),("freq",midiCPS note)])
  
-- $triggeringByGUI
--
-- Pausing sequence with pushing a button. The button is made with
-- using GUI from GTK.
--
runByGUI :: IO ()
runByGUI = do
  initGUI
  guiContents >>= guiContainer
  mainGUI
  

-- Contents of the gui.
-- guiContents :: WidgetClass w => IO w
guiContents = do
  container <- vBoxNew True 10
  return container

-- | Container of the gui.
guiContainer :: WidgetClass w => w -> IO ()
guiContainer widget = do
  window <- windowNew
  set window [containerBorderWidth := 10,
              containerChild := widget]
  window `onDestroy` mainQuit
  widgetShowAll window

-- $triggeringBySignalAmplitude
--
-- A bit trickier, in sclang.
--
runBySignalAmplitude :: IO ()
runBySignalAmplitude = undefined