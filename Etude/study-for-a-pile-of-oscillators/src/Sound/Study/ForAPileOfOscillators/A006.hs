{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with a pile of oscillators, take 6.
--
-- Classic example of UPIC-like granular synthesis.
-- Using image to control amplitude envelope of oscillators.
-- Oscillators frequencies are determined by image's y axis.
-- Height of input image file need to be 256.
--
-- List related functions are imported from Data.List.Stream.
-- This makes the code 2 to 3 times faster. NoImplicitPrelude language pragma
-- is for avoiding conflicts from this module.
--
-- Not sure how to speedup with using multiple cores.
-- So far, increasing cores slow down writing OSC score file.
--
-- Without using any functions from Control.Parallel and using list fusion,
-- writing OSC file for 256x256 PPM image is around 4.3 seconds.
-- Doing @parMap rpdeepseq@ and @parMap rpar@ inside @applyToPixmap@ function,
-- writing OSC file with single core take around 7 seconds, 2 cores is 5 secs.
--
module Sound.Study.ForAPileOfOscillators.A006 where

import Prelude
  ( Num(..), Fractional(..), Floating(..), Enum(..), Eq(..), Ord(..)
  , FilePath, IO, Int
  , (||), (&&), (^)
  , putStr, error, even, otherwise
  , fromRational, fromIntegral, show )

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Array
import Data.Either
import Data.Function
import Data.Word (Word8)
import Data.Tuple
import qualified Data.ByteString.Lazy as L

-- import Control.DeepSeq
-- import Control.Parallel
-- import Control.Parallel.Strategies

import Codec.PBM.Parser
import Codec.PBM.Types
import Data.List.Stream
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.Study.ForAPileOfOscillators.Common

-- | Unused.
main :: IO ()
main = putStr "No gui for A006"

setup :: (Transport t) => t -> IO OSC
setup fd = do
  writeSynthdef "ac6" ac6
  reloadSynthdef fd

-- | Write score from pgm image file.
writePGMScore :: FilePath -> FilePath -> IO ()
writePGMScore src dest = do
  dat <- getPGM src
  let os = zipWith f [1..] (transpose $ map mkOSC dat)
      f t ms = map (Bundle (NTPr (t*timeScale)) . (:[])) ms
      ini = map (Bundle (NTPr 0) . (:[])) $ (treeToNew 0 a006Nodes ++ initOSC)
  writeNRT dest $ ini ++ concat os

-- | Write score from ppm image file.
writePPMScore :: FilePath -> FilePath -> IO ()
writePPMScore src dest = do
  arr <- getPPM src
  let os = zipWith f [1..] . transpose .  applyToPixmap f3 $ arr
      f t ms = map (\m -> Bundle (NTPr (t*timeScale)) [m]) ms
      ini = map (Bundle (NTPr 0) . (:[])) $ (treeToNew 0 a006Nodes ++ initOSC)
  writeNRT dest $ ini ++ concat os

a006Nodes :: SCNode
a006Nodes =
  grp 1
    [grp 10 []
    ,grp 11 [syn hitId "ac6" ["amp":=ampScale,"ts":=timeScale]]
    ,grp 12 oscs]

hitId = 1100

timeScale = 0.03
ampScale = 0.05
freqOffset = 50
freqScale = 0.985
panDist = 0.35

initOSC :: [OSC]
initOSC = map f oscIds
  where
    f i = c_set [(freqBus i,fd+freqOffset),(panBus i,pan)]
      where
        fd = exp (log (24000*freqScale) * (i'/256))
        i' = fromIntegral (i - 20001)
        pan = if even i then f i else negate (f i)
        f j = (panDist*i'/256)

ac6 :: UGen
ac6 = ac6' ("amp"=:0.1) ("ts"=:timeScale)
ac6' amp ts = mrg $ map mkO oscIds
  where
    mkO i = out (fromIntegral $ ampBus i) . (* amp) . (* ampi) .
            envGen kr trgi 1 0 duri DoNothing $
            env [0,0,1,0] [0,atki,1-atki] [EnvNum crvi] (-1) 0
      where
        ampi = (("amp_"++i')=:1)
        trgi = (("t_trig_"++i')=:0)
        atki = (1-("atk_"++i')=:2e-4)
        crvi = (1-("crv_"++i')=:0.5) * 26 - 13
        duri = (1-("dur_"++i')=:0.5) * 2 * ts + 1e-4
        i' = show i

-- | Type synonym for a function to get OSC data from each pixel.
type RGBFunc
  = Int   -- ^ Y axis
 -> Word8 -- ^ Red, from 0 to 255
 -> Word8 -- ^ Green, from 0 to 255
 -> Word8 -- ^ Blue, from 0 to 255
 -> OSC

-- | The actual function that converting RGB data to OSC message of this piece.
f3 :: RGBFunc
f3 i r g b = n_set hitId ms
  where
    ms | r > 0 || g > 0 || b > 0 =
      [("amp_"++i', fromIntegral (r+g+b)/765),("atk_"++i', fromIntegral r/255)
      ,("crv_"++i', fromIntegral g/255),("dur_"++i', fromIntegral b/255)
      ,("t_trig_"++i',1)]
       | otherwise               = []
    i' = show (oscIds!!i)

getPGM :: FilePath -> IO [(Int, [Word8])]
getPGM file = do
  c <- L.readFile file
  case parse parsePGM c of
    Right gm -> return $ sep256 gm
    Left err -> error err

-- | Parse given PPM image file and return its contents.
getPPM :: FilePath -> IO Pixmap
getPPM file = do
  c <- L.readFile file
  case parse parsePPM c of
    Right pm -> return $ pm
    Left err -> error err

-- | Applys given RGBFunc to Pixmap.
applyToPixmap :: RGBFunc -> Pixmap -> [[OSC]]
applyToPixmap f a =
  map (map (\((y,x), (r,g,b)) -> f (inv y) r g b)) .
  groupBy ((==) `on` (fst . fst)) . assocs $ a
  where
    inv i = let (_,(m,_)) = bounds a in m - i

-- | Make OSC for each row of image.
--
-- Used with PGM image.
--
mkOSC :: (Int, [Word8]) -> [OSC]
mkOSC (i,ws) = map f ws
  where
    f w =
      n_set hitId [("t_trig_"++show (oscIds!!i),1)
                  ,("amp_"++show (oscIds!!i), fromIntegral w/255)]

-- | Extract, repack, and convert values from pgm image.
--
-- Value of Word8 is inverted, (i.e. black is closer to max, white is 0)
--
sep256 :: Greymap -> [(Int,[Word8])]
sep256 gm = go 256 (greyData gm)
  where
    go n bs
      | L.null bs = []
      | otherwise = (n,L.unpack (L.map f cs)):go (pred n) rest
      where
        (cs,rest) = L.splitAt (fromIntegral $ greyWidth gm) bs
    f x = fromIntegral (greyMax gm) - x
