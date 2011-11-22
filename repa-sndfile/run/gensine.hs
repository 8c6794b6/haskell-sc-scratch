{-# LANGUAGE TypeOperators #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Generates sine wave.

-}
module Main where

import System.Environment (getArgs)
import Data.Array.Repa ((:.)(..), Array, DIM2, Z(..), force, fromFunction)
import Data.Array.Repa.IO.Sndfile

import qualified Test.ReadWrite as T

import Sound.File.Sndfile.Buffer.Vector
import qualified Data.Vector.Storable as V
import qualified Sound.File.Sndfile as S

main :: IO ()
main = do
  args <- getArgs
  case args of
    dur:frq:path:mode:_ ->
      let dur' = read dur
          frq' = read frq
          fmt  = waveMonoPcm16 dur'
      in  case mode of
        "buf" -> writeSF path fmt (force $ genSine dur' frq')
        "raw" -> T.write_arr path fmt (T.genSine dur' frq')
        "vec" -> S.writeFile fmt path (genSine' dur' frq') >> return ()
        _     -> usage
    _ -> usage

usage :: IO ()
usage = error "Usage: duration freq path [buf|raw|vec]"

-- | Generates sine wave.
genSine ::
  Int       -- ^ Duration in seconds.
  -> Double -- ^ Frequency
  -> Array DIM2 Double
genSine dur frq =
  let sh = Z :. 1 :. (dur * 48000)
  in  fromFunction sh $ \(_:._:.j) -> sin (frq * fromIntegral j * pi * 2 / 48000)
{-# INLINE genSine #-}

-- | Wave single-channel PCM 16bit header format.
waveMonoPcm16 ::
  Int -- ^ Duration in seconds.
  -> Info
waveMonoPcm16 dur = Info
  { samplerate = 48000
  , frames = 48000 * dur
  , channels = 1
  , format = Format HeaderFormatWav SampleFormatPcm16 EndianFile
  , sections = 1
  , seekable = True }

{- ---------------------------------------------------------------------------
 -
 - Write stereo sound, using different frequency for each channel.
 - Modify header format to 'channels = 2'
 -

genSine :: Int -> Double -> Array DIM2 Double
genSine dur frq =
  let sh = Z :. 2 :. (dur * 48000) :: DIM2
  in  fromFunction sh $ \(_ :. i :. j) ->
        sin (frq * (fromIntegral i + 1) * fromIntegral j * pi * 2 / 48000)
-}

-- |
-- Generating 30 seconds sine 440 hz sine wav took about 0.2 sec with repa.
-- In 8 core machine, using 8 threads, and 4x more memory than result file.
--
-- For just reading and writing sound files as haskell data, storable vector
-- take almost same execution time. It took about 0.25sec in same machine,
-- memory usage was less.
--
genSine' :: Int -> Double -> Buffer Double
genSine' dur frq =
  toBuffer $
  V.generate (48000 * dur) $ \i ->
  sin (frq * fromIntegral i * pi * 2 / 48000)
{-# INLINE genSine' #-}
