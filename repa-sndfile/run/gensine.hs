{-# LANGUAGE TypeOperators #-}
module Main where

import System.Environment (getArgs)
import Data.Array.Repa ((:.)(..), Array(..), DIM2, Z(..), fromFunction)
import Data.Array.Repa.IO.Sndfile

main :: IO ()
main = do
  args <- getArgs
  case args of
    dur:frq:path:_ ->
      let dur' = read dur
          frq' = read frq
      in  writeSF path (waveMonoPcm16 dur') (genSine dur' frq')
    _              -> error "Usage: duration freq path"

genSine :: Int -> Double -> Array DIM2 Double
genSine dur frq =
  let sh = Z :. 2 :. (dur * 48000) :: DIM2
  in  fromFunction sh $ \(_ :. i :.j) ->
        sin (frq * (fromIntegral i + 1) * fromIntegral j * pi * 2 / 48000)

waveMonoPcm16 :: Int -> Info
waveMonoPcm16 dur = Info
  { samplerate = 48000
  , frames = 48000 * dur
  , channels = 2
  , format = Format HeaderFormatWav SampleFormatPcm16 EndianFile
  , sections = 1
  , seekable = True }
