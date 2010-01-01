------------------------------------------------------------------------------
-- | Module for notes with amplitude, frequency, and duration.
-- 

module SimpleNotes where

data Note = Note { notePitch :: Double,
                   noteAmp :: Double,
                   noteDur :: Double }
          deriving (Eq, Show)

notes1 :: [Note]
notes1 = concat $ replicate 2
    -- [
    --  Note 60 80 0.5, Note 62 78 0.5, Note 64 78 1.0, Note 60 81 1.5,
    --  Note 62 76 0.5, Note 64 81 2.0, Note 60 80 1.5, Note 62 81 0.5
    -- ]
         [
          Note 60 80 1.5, Note 62 78 0.5, Note 64 78 1.5, Note 65 81 0.5,
          Note 67 80 1.0, Note 69 78 1.0, Note 71 78 1.0, Note 72 81 1.0
         ]

notes2 :: [Note]
notes2 =
    [
     Note 48 82 1.5, Note 43 75 0.5, Note 48 80 1.5, Note 43 75 0.5,
     Note 53 81 1.5, Note 48 75 0.5, Note 53 80 1.5, Note 48 75 0.5,
     Note 48 82 1.5, Note 43 75 0.5, Note 48 80 1.5, Note 43 75 0.5,
     Note 55 81 1.5, Note 50 75 0.5, Note 55 80 1.5, Note 50 75 0.5
    ]
