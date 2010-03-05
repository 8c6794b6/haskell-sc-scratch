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
    playNote,

    -- * Triggering a pattern by a GUI
    -- $triggeringByGUI
    runByGUI,

    -- * Triggering a pattern by signal amplitude
    -- $triggeringBySignalAmplitude
    runBySignalAmplitude

    ) where

import Control.Applicative
import Control.Monad
import Data.Char

import Sound.OpenSoundControl
import Sound.SC3

import Reusable
import SCQuery
import SCTree
import SCSched

main :: IO ()
main = undefined

-- $controlByHID
--
-- Translation of @Phidkey@ and @PhidSlot@ pattern in sclang.
-- Triggering sending of message with key press. 'a' is midiNote 60,
-- and 'w' is 61, as so on until 'k', 72.
--
runByHID :: IO ()
runByHID = getChar >>= playByKey >> runByHID

playByKey :: Char -> IO ()
playByKey 'a' = playNote 60
playByKey 'w' = playNote 61
playByKey 's' = playNote 62
playByKey 'e' = playNote 63
playByKey 'd' = playNote 64
playByKey 'f' = playNote 65
playByKey 't' = playNote 66
playByKey 'g' = playNote 67
playByKey 'y' = playNote 68
playByKey 'h' = playNote 69
playByKey 'u' = playNote 70
playByKey 'j' = playNote 71
playByKey 'k' = playNote 72
playByKey 'q' = return ()
playByKey _ = return ()

playNote :: Double -> IO ()
playNote note = withSC3 $ \fd -> do
   send fd (s_new "simpleSynth" (-1) AddToTail 1
            [("dur",1),("freq",midiCPS note)])
  
-- $triggeringByGUI
--
-- Using GTK. Triggering sending of
--
runByGUI :: IO ()
runByGUI = undefined

-- $triggeringBySignalAmplitude
--
-- A bit trickier, in sclang.
--
runBySignalAmplitude :: IO ()
runBySignalAmplitude = undefined