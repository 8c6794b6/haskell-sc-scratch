{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (Rank2Types, FlexibleContexts)
--
-- Interpreter of pattern DSL.
--
module Sound.SC3.Lepton.Pattern.Interpreter
  ( -- * Running patterns
    R(..), runP, runPIO, foldPIO, foldPIO_, mapPIO_,

    -- * Showing patterns
    S(..), showP,

    -- -- Viewing patterns
    -- V(..), viewP,
  ) where

-- import Sound.SC3.Lepton.Pattern.Expression
-- import Sound.SC3.Lepton.Pattern.Interpreter.Fusion
import Sound.SC3.Lepton.Pattern.Interpreter.R
import Sound.SC3.Lepton.Pattern.Interpreter.S

