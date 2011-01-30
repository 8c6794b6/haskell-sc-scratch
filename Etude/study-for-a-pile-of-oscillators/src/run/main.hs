{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Command line wrapper writing OSC score file and showing gui.
--
module Main where

import System.Console.CmdArgs

import qualified Sound.Study.ForAPileOfOscillators.A001 as A001M
import qualified Sound.Study.ForAPileOfOscillators.A002 as A002M
import qualified Sound.Study.ForAPileOfOscillators.A003 as A003M
import qualified Sound.Study.ForAPileOfOscillators.A004 as A004M
import qualified Sound.Study.ForAPileOfOscillators.A005 as A005M
import qualified Sound.Study.ForAPileOfOscillators.A006 as A006M

main :: IO ()
main = do
  arg <- cmdArgs $ modes 
           [Score "a001.sco" A001
           ,GUI A001
           ,ReadImage "out.sco" "input.ppm"]
  case arg of
    Score path p  -> writeScore path p
    GUI p         -> showGui p
    ReadImage i o -> procImage i o

data SFAPOO
  = Score {outFile :: FilePath, piece :: Piece}
  | GUI {piece :: Piece}
  | ReadImage { inFile :: FilePath, outFile :: FilePath } -- only for A006
  deriving (Eq, Show, Data, Typeable)

data Piece = A001 | A002 | A003 | A004 | A005
           deriving (Eq, Show, Data, Typeable, Enum)

writeScore :: FilePath -> Piece -> IO ()
writeScore path p = do
  putStrLn $ "Writing score of " ++ show p ++ " to " ++ path
  case p of
    A001 -> A001M.writeA001Score path
    A002 -> A002M.writeA002Score path
    A003 -> A003M.writeA003Score path
    A004 -> A004M.writeA004Score path
    A005 -> A005M.writeA005Score path
    -- _    -> error "not yet written"

showGui :: Piece -> IO ()
showGui p = do
  putStrLn $ "Showing gui of " ++ show p
  case p of
    A001 -> A001M.main
    A002 -> A002M.main
    A003 -> A003M.main
    A004 -> A004M.main
    A005 -> A005M.main
    -- _    -> error "not yet written"

procImage :: FilePath -> FilePath -> IO ()
procImage src dest = A006M.ws2 src dest