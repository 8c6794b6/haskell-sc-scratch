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
    toggleThread,
    runSendingMessage,

    -- * Triggering a pattern by signal amplitude
    -- $triggeringBySignalAmplitude
    runBySignalAmplitude

    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.State
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
-- This action is using @unsafeInitGUIForThreadedRTS@, instead of
-- @initGUI@. Need to compile this with @-threaded@ option.
--
runByGUI :: IO ()
runByGUI = do
  unsafeInitGUIForThreadedRTS
  guiContents >>= guiContainer
  mainGUI

-- | Contents of the gui.
guiContents :: IO VBox
guiContents = do
  -- MVars and threads
  var1 <- newEmptyMVar
  tId1 <- forkIO (forever $ runSendingMessage 440 var1)

  var2 <- newEmptyMVar
  tId2 <- forkIO (forever $ runSendingMessage 660 var2)

  -- buttons for toggling thread
  toggleButton1 <- buttonNew
  set toggleButton1 [buttonLabel := "toggle thread 1"]
  toggleButton1 `onClicked` (toggleThread var1)

  toggleButton2 <- buttonNew
  set toggleButton2 [buttonLabel := "toggle thread 2"]
  toggleButton2 `onClicked` (toggleThread var2)         
  
  -- button for quitting the gui
  quitButton <- buttonNew
  set quitButton [buttonLabel := "quit"]
  quitButton `onClicked` (mainQuit >> killThread tId1 >> killThread tId2)

  -- container of buttons
  box <- vBoxNew True 10
  set box [containerChild := toggleButton1,
           containerChild := toggleButton2,
           containerChild := quitButton]
  return box

-- | Toggles the thread that sending message to scsynth.
toggleThread :: MVar () -> IO ()
toggleThread var = do
  isEmpty <- isEmptyMVar var
  putStrLn $ "from toggleThread: " ++ show isEmpty
  if isEmpty
     then putMVar var ()
     else takeMVar var

-- | Send osc message.
runSendingMessage :: Double -> MVar () -> IO ()
runSendingMessage frq var = do
  putMVar var ()
  withSC3 $ \fd -> send fd $
                   s_new "simpleSynth" (-1) AddToTail 1 [("freq",frq)]
  a <- takeMVar var
  threadDelay (round $ 10 ^ 6 * 0.5)

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