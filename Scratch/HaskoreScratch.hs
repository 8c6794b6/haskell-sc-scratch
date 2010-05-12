module HaskoreScratch where

import Data.Accessor (getVal)
import Haskore
import Haskore.Basic.Duration
import Haskore.Melody
import Haskore.Music
import Haskore.Basic.Pitch (toInt)
import Medium.Controlled.List (parallel, serial, T(..))
import qualified Medium.Controlled.List as MCL

import Sound.OpenSoundControl
import Sound.SC3 (s_new, midiCPS, AddAction(..), withSC3, reset)
import Sound.SC3.Wing.ScheduleSimple

type MelodyT a = Haskore.Melody.T a
type ListT = MCL.T

-- | Type synonym for function to convert a Note to list of OSC. 
type NoteToOSCs a = Double -- ^ Duration
                  -> Int -- ^ Pitch, in MIDI
                  -> Maybe a -- ^ Note attribute
                  -> [OSC] 

noteToSeqEv :: NoteToOSCs a -> MelodyT a -> SeqEvent OSC
noteToSeqEv f n@(Primitive x) = 
     [(noteToBeat n, f (noteToBeat n) (toInt $ noteToPitch n) (noteToAttr n))]
noteToSeqEv f n@(Serial xs) = concatMap (noteToSeqEv f) xs
noteToSeqEv f n@(Parallel xs) = foldr (//) [] (map (noteToSeqEv f) xs)
noteToSeqEv f n@(Control x y) = []

seri :: [MCL.T control a] -> MCL.T control a
seri = serial 

para :: [MCL.T control a] -> MCL.T control a
para = parallel

noteToAttr :: MelodyT a -> Maybe a
noteToAttr (Primitive x) = fmap (getVal noteAttrs) x' where Atom _ x' = x
noteToAttr _ = Nothing

noteToBeat :: Fractional a => MelodyT b -> a
noteToBeat (Primitive x) = 4 * toNumber d where Atom d _ = x


f1 :: NoteToOSCs a
f1 _ pch _ = [s_new "simplePitched" (-1) AddToTail 1 
              [("freq", midiCPS $ fromIntegral pch)]]


aaa :: MelodyT [(String,Double)]
aaa = seri 
      [e 5 qn [("amp", 80)],
       f 5 qn [("amp", 80)],
       g 5 qn [("amp", 80)],
       para
        [e 4 qn [("amp", 80)],
         b 4 qn [("amp", 80)],
         e 5 qn [("amp", 80)]]]

line1 :: MelodyT ()
line1 = para [l1, l2, l3]
    where
      l1 = seri [e 7 qn (), d 7 qn (), c 7 qn ()]
      l2 = seri [a 6 qn (), g 6 qn (), g 6 qn ()]
      l3 = seri [c 5 qn (), b 4 qn (), c 5 qn ()]

line2 :: MelodyT ()
line2 = para [l1, l2, l3]
    where
      l1 = seri [e 6 sn (), d 6 sn (), c 6 sn (), d 6 sn ()]
      l2 = seri [g 5 en (), g 5 en ()]
      l3 = seri [c 4 qn ()]

