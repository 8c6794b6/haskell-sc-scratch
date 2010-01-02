------------------------------------------------------------------------------
-- | Module for notes with amplitude, frequency, and duration.
--

module SimpleNotes where

import System.Random
import Sound.SC3

data Note = Note { notePitch :: Double,
                   noteAmp :: Double,
                   noteDur :: Double }
          deriving (Eq, Show)

noteToFreq :: Note -> Double
noteToFreq = midiCPS . notePitch

noteToAmp :: Note -> Double
noteToAmp = dbAmp . (flip (-) 100) . noteAmp

noteToDur :: Note -> Double
noteToDur = noteDur

notes1 :: [Note]
notes1 = -- concat $ replicate 2
    [
     Note 60 80 0.5, Note 62 78 0.5, Note 64 78 1.0, Note 60 81 1.5,
     Note 62 76 0.5, Note 64 81 2.0, Note 60 80 1.5, Note 62 81 0.5
    ]
         -- [
         --  Note 60 80 1.5, Note 62 78 0.5,
         --  Note 64 78 1.5, Note 65 81 0.5,
         --  Note 67 80 1.0, Note 69 78 1.0,
         --  Note 71 78 1.0, Note 72 81 1.0
         -- ]

notes2 :: [Note]
notes2 =
    [
     Note 48 82 1.5, Note 43 75 0.5, Note 48 80 1.5, Note 43 75 0.5,
     Note 53 81 1.5, Note 48 75 0.5, Note 53 80 1.5, Note 48 75 0.5,
     Note 48 82 1.5, Note 43 75 0.5, Note 48 80 1.5, Note 43 75 0.5,
     Note 55 81 1.5, Note 50 75 0.5, Note 55 80 1.5, Note 50 75 0.5
    ]

-- notes1 = take 3000 $ zipWith3 Note pitches amps durs where
--     gen = mkStdGen 298588
--     pitches = map (fromIntegral . unPentaPitch) $
--               randomRs (PentaPitch 60,PentaPitch 72) gen
--     amps = randomRs (75,80) gen
--     durs = map ((* 0.5) . fromIntegral) $ randomRs (1,4::Int) gen

data PentaPitch = PentaPitch {unPentaPitch::Int} deriving (Eq, Show)

instance Random PentaPitch where
    randomR (min,max) g = (PentaPitch v', g')
        where (v,g') = randomR (0,2::Int) g
              v' = case v of
                     0 -> 60
                     1 -> 62
                     2 -> 64
    random = randomR (PentaPitch 0,PentaPitch 0)
