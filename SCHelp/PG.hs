------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- Pattern Cookbook of supercollider help file.
--

module SCHelp.PG 
    (module SCHelp.PG.Cookbook01,
     module SCHelp.PG.Cookbook02,
     module SCHelp.PG.Cookbook03,
     module SCHelp.PG.Cookbook05,
     module SCHelp.PG.Cookbook06,
     module SCHelp.PG.Cookbook07,
     module SCHelp.PG.Cookbook072
    ) where

import SCHelp.PG.Cookbook01
import SCHelp.PG.Cookbook02 hiding (main)
import SCHelp.PG.Cookbook03 hiding (main)
import SCHelp.PG.Cookbook05 
import SCHelp.PG.Cookbook06
import SCHelp.PG.Cookbook07 hiding 
    (getDurs, 
     mkEvent,
     rhythmDelta,
     getRestIndices,
     hhBase,
     kikBase,
     snrBase )
import SCHelp.PG.Cookbook072