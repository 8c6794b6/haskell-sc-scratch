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
-- Wrapper for gui cli.
--
module Main where

import Data.Generics (Data, Typeable)
import qualified Data.Map as M

import Sound.SC3 (withSC3)
import Sound.SC3.Lepton (mkTree)
import Sound.SC3.Lepton.GUI
import System.Console.CmdArgs (cmdArgs, modes)

import qualified Pssd.Engineering.Motors as Motors
import qualified Pssd.Future.Droids as Droids
import qualified Pssd.Monster.Birds as Birds

-- | Data type to choose SCTree from other Pssd modules.
data Pssd
  = Motors
  | Droid
  | Birds
    deriving (Eq, Show, Enum, Data, Typeable)

main :: IO ()
main = do
  arg <- cmdArgs $ modes [Motors .. Birds]
  withSC3 $ \fd -> do
    let go tree = mkTree tree fd >> treeToGui tree hints fd
    case arg of
      Motors -> go Motors.motorGraph
      Droid  -> go Droids.droidGraph
      Birds  -> go Birds.b2Tree

hints :: Hints
hints = M.fromList
  [("turnMotor",
    [ParamRange "freq" 0 8])
  ,("motor3",
    [ParamRange "amp" 0 1
    ,ParamRange "top" 100 800
    ,ParamRange "optime" 0.01 1.0
    ,ParamRange "curve" (-8) 8
    ,ParamRange "res" 0.05 0.98
    ,ParamRange "brushSize" 0.01 0.98
    ,ParamRange "brushLevel" 0.1 5
    ,ParamRange "rotor" 0.01 1])
  ,("hitDroid",
    [ParamRange "freq" 0 8])
  ,("droid",
    [ParamRange "period" 0 8])]
  `M.union`
  Birds.b2Hints
