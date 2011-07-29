------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with Shellac package.
--
-- From:
--
-- * http://gimbo.org.uk/blog/2009/09/15/a-haskell-shell-for-computing-musical-scales-in-58-lines-of-code/
--
module Repl01 where

import Data.List
import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

import Text.PrettyPrint.HughesPJ
import Text.Regex

main :: IO ()
main = runShell (mkShellDescription [] react) haskelineBackend ()

react :: String -> Sh () ()
react = mapM_ (uncurry printScale) . checkFit . parseNotes where
  printScale :: String -> [Note] -> Sh () ()
  printScale n ns = shellPutStrLn (n ++ ": " ++ ppNotes ns)

type Interval = Int
type Note = Int
type NoteName = String
data Scale = Scale { scaleName :: String
                   , scaleIntervals :: [Interval]
                   } deriving (Eq, Show)

knownScales :: [Scale]
knownScales =
  [Scale "Major" [0, 2, 4, 5, 7, 9, 11]
  ,Scale "Minor" [0, 2, 3, 5, 7, 8, 10]
  ,Scale "Harmonic Minor" [0, 2, 3, 5, 7, 8, 11]
  ,Scale "Major Pentatonic" [0, 2, 4, 7, 9]
  ,Scale "Relative Minor Pentatonic" [0, 3, 5,  7, 10]
  ,Scale "Yo" [0, 2, 5, 7, 9] ]

noteMod :: Note -> Note
noteMod n = n `mod` 12

scaleNotes :: Note -> Scale -> [Note]
scaleNotes base (Scale _ scale) =
  nub . sort . map noteMod $ zipWith (+) (repeat base) scale

chromatic :: [NoteName]
chromatic = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

namedNote :: NoteName -> Note
namedNote name = case elemIndex name chromatic of
  Just a  -> a
  Nothing -> error $ "unknown note " ++ name

noteName :: Note -> NoteName
noteName note = chromatic !! noteMod note

mkScales :: [Note] -> [Scale] -> [(String, [Note])]
mkScales notes scales =
  [(name,note') | note <- notes, scale <- scales
                , let name = noteName note ++ " " ++ scaleName scale
                , let note' = scaleNotes note scale]

allScales :: [(String, [Note])]
allScales = mkScales [0..11] knownScales

parseNotes :: String -> [Note]
parseNotes = map namedNote . splitRegex (mkRegex " +")

ppNotes :: [Note] -> String
ppNotes = show . hsep . map (text . noteName) . nub . sort . map noteMod

checkFit :: [Note] -> [(String, [Note])]
checkFit notes = filter ((notes `isSubList`) . snd) allScales where
  isSubList :: (Eq a) => [a] -> [a] -> Bool
  isSubList x y = null $ x \\ y
