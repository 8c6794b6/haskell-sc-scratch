{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (Rank2Types, FlexibleContexts)
--
-- Pattern interpreter with state monad.
--

module Sound.SC3.Lepton.PState where  

import Data.Map (Map)
import qualified Data.Map as M
import System.Random
  
import "mtl" Control.Monad.State
import Sound.SC3.Lepton.Pattern

------------------------------------------------------------------------------
-- 
-- Pattern interpreter with state monad
--
------------------------------------------------------------------------------

newtype PS a = PS {unPS :: State (PState a) [a]}

evalP p st = evalState (unPS p) st
evalP' p g = evalState (unPS p) (PState M.empty g)
evalPIO p = return . evalP' p =<< newStdGen

data PState a = PState { psEnv :: Map String a
                       , psGen :: StdGen }  

instance Pval PS where
  pval a = PS $ return [a]
  
instance Pempty PS where  
  pempty = PS $ return []
  
instance Plist PS where  
  plist = PS . return
  
