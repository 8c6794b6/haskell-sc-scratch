------------------------------------------------------------------------------
-- |
-- Module      : Sound.SC3.Wing.MIDI
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- MIDI Extension to hsc3.
--

module Sound.SC3.Wing.MIDI
    ( withMIDI
    ) where


import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Control.Monad (forever)
import Sound.Alsa.Sequencer

-- | Like @withEvents@ found in Sound.ALSA.Sequencer module, invoke
-- given action with MIDI input event.
-- Event datatype used here is from Sound.Alsa package.
--
withMIDI :: String  -- ^ Name of client exposed to alsa
         -> (Event -> IO a) -- ^ Action invoked with midi events.
         -> IO a
withMIDI clientName action = do
  bracket (getSeq clientName) close worker
    where
      worker aseq = do
        _ <- getWriteablePort aseq "input"
        forever (action =<< event_input aseq)

-- | Get sequencer handler with given client name.
getSeq :: String -> IO SndSeq
getSeq cname = do
  aseq <- open default_seq_name open_input Block
  set_client_name aseq cname
  return aseq

-- | Get input port for given sequencer client.
getWriteablePort :: SndSeq -> String -> IO Port
getWriteablePort s name =
    create_simple_port s name cap type_midi_generic
        where cap = caps [cap_write, cap_subs_write, cap_sync_write]
