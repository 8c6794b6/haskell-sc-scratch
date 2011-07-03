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
-- Main entry poing for tutorial executables.
--
module Main where

import Data.List (intersperse)
import System.Exit

import System.Console.CmdArgs

import qualified GLTut.RollingRectangle as RR
import qualified GLTut.RollingRectangle2 as RR2
import qualified GLTut.Fountain as FN
import qualified GLTut.Commet as CM
import qualified GLTut.AlphaBlend as AB
import qualified GLTut.RenderString as RS

data GLTut = GLTut { prog :: Maybe Prog }
              deriving (Eq, Show, Data, Typeable)

data Prog = RR | RR2 | FN | CM | AB | RS
           deriving (Eq, Show, Data, Typeable, Enum)

progs :: [Prog]
progs = [RR .. ]

defaultArg :: GLTut
defaultArg = GLTut { prog = def &= help progNames } where
  progNames = "One of: " ++ (concat $ intersperse ", " (map show progs))

main :: IO ()
main = do
  GLTut info <- cmdArgs defaultArg
  maybe (selectTutName >> exitFailure) (\x -> go x >> exitSuccess) info

go :: Prog -> IO ()
go p = case p of
  RR  -> RR.main
  RR2 -> RR2.main
  FN  -> FN.main
  CM  -> CM.main
  AB  -> AB.main
  RS  -> RS.main

selectTutName :: IO ()
selectTutName = do
  putStrLn "prog must be one of:"
  mapM_ (putStrLn . ("  " ++) . show) progs
