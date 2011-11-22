{-# LANGUAGE TypeOperators #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Tests for generating sine wave sound.

-}
module Test.GenSine where

import Data.Array.Repa ((:.)(..), Array(..), DIM2, Z(..))
import qualified Data.Array.Repa as R

import Data.Array.Repa.IO.Sndfile

-- ---------------------------------------------------------------------------
-- Sine wave generator

genSine :: Int -> Double -> Array DIM2 Double
genSine dur frq =
  let sh = Z :. 2 :. (dur * 48000) :: DIM2
  in  R.fromFunction sh $ \ix@(_ :. c :._) ->
        sin (frq * (fromIntegral c + 1) * fromIntegral (R.toIndex sh ix) * pi * 2 / 48000)

writeSine :: Int -> Double -> FilePath -> IO ()
writeSine dur frq o = writeSF o (waveMonoPcm32 dur) (genSine dur frq)

-- ---------------------------------------------------------------------------
-- Sound format

waveMonoPcm32 :: Int -> Info
waveMonoPcm32 dur = Info
  { samplerate = 48000
  , frames = 48000 * dur
  , channels = 2
  , format = Format HeaderFormatWav SampleFormatPcm16 EndianFile
  , sections = 1
  , seekable = True }
