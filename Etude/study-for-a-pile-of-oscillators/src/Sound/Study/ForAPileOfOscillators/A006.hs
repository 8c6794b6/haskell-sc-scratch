{-# LANGUAGE NoMonomorphismRestriction #-}
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
-- Using image as input data for oscillators.
-- Input image file width need to be 256.
--
module Sound.Study.ForAPileOfOscillators.A006 where

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Array
import Data.Function (on)
import Data.List (transpose, groupBy)
import Data.Word (Word8)
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8

import Codec.PBM.Parser
import Codec.PBM.Types
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.Study.ForAPileOfOscillators.Common

-- | Write osc score to given filepath.
main :: IO ()
main = do
  args <- getArgs
  let dest | null args = "./out.osc"
           | otherwise = head args
  ws2 out_2 dest

setup :: (Transport t) => t -> IO OSC
setup fd = do
  writeSynthdef "ac6" ac6
  reloadSynthdef fd

go :: (Transport t) => t -> IO ()
go = goFrom 0

goFrom :: (Transport t) => Int -> t -> IO ()
goFrom frm fd = do
  now <- utcr
  dat <- getData imageFile
  send fd $ Bundle (UTCr now) (sync 1:treeToNew 0 a006Nodes)
  wait fd "/synced"
  send fd $ Bundle (UTCr now) initOSC
  threadDelay (10^6)
  forM_ (transpose $ map mkOSC dat) $ \ms -> do
    send fd $ Bundle immediately ms
    threadDelay $ floor $ timeScale * 1e6

writeA006Score :: FilePath -> IO ()
writeA006Score = writeScoreOf imageFile

writeScoreOf :: FilePath -> FilePath -> IO ()
writeScoreOf src dest = do
  dat <- getData src
  let os = zipWith f [1..] (transpose $ map mkOSC dat)
      f t ms = map (Bundle (NTPr (t*timeScale)) . (:[])) ms
      ini = map (Bundle (NTPr 0) . (:[])) $ (treeToNew 0 a006Nodes ++ initOSC)
  writeNRT dest $ ini ++ concat os

-- | Write score from ppm image file.
ws2 :: FilePath -> FilePath -> IO ()
ws2 src dest = do
  arr <- getPPM src
  let os = zipWith f [1..] . transpose .  applyToPixmap f2 $ arr
      f t ms = map (\m -> Bundle (NTPr (t*timeScale)) [m]) ms
      ini = map (Bundle (NTPr 0) . (:[])) $ (treeToNew 0 a006Nodes ++ initOSC)
  writeNRT dest $ ini ++ concat os

a006Nodes :: SCNode
a006Nodes =
  grp 1
    [grp 10 []
    ,grp 11 [syn hitId "ac6" ["amp":=ampScale,"rel":=timeScale*durScale]]
    ,grp 12 oscs]

grp = Group
syn = Synth
hitId = 1100

timeScale = 0.03
durScale = 0.8
ampScale = 0.05
freqOffset = 50
freqScale = 0.985
panScale = 0.35

imageFile :: FilePath
imageFile = lenna

uniitiled_1 = "/home/atsuro/images/pgm/Untitled.pgm"  -- 640x256
untitiled_5  = "/home/atsuro/images/pgm/untitled5.pgm" -- 1024x256
untitiled_52 = "/home/atsuro/images/pgm/untitled5_2.pgm" -- 1024x256
mandelbrot256 = "/home/atsuro/images/pgm/mandelbrot_256.pgm" -- 256x256
mandelbrot1024 = "/home/atsuro/images/pgm/mandelbrot_1024x256.pgm" -- 1024x256
lenna = "/home/atsuro/images/pgm/lenna_1024x256.pgm" -- 1024x256

mandelA = "/home/atsuro/images/pgm/a.pgm" -- 800x256
capture2 = "/home/atsuro/images/pgm/capture2.pgm" -- 800x256
capture3 = "/home/atsuro/images/pgm/capture3.pgm" -- 701x256
bsd = "/home/atsuro/images/pgm/bsd.pgm" -- 2048x256

-- ppm
lennaPPM = "/home/atsuro/images/ppm/lenna256.ppm" -- 512x256
bsdPPM = "/home/atsuro/images/ppm/bsd.ppm"
bsdbbgPPM = "/home/atsuro/images/ppm/bsd_black_bg.ppm"
out2PPM = "/home/atsuro/images/ppm/out2.ppm"
out3PPM = "/home/atsuro/images/ppm/out3.ppm"
untitled5PPM = "/home/atsuro/images/ppm/untitled5.ppm"
untitled7PPM = "/home/atsuro/images/ppm/untitled7.ppm"
untitled8PPM = "/home/atsuro/images/ppm/untitled8.ppm"
capture4 = "/home/atsuro/images/ppm/capture4.ppm"
capture5 = "/home/atsuro/images/ppm/capture5.ppm"
capture6 = "/home/atsuro/images/ppm/capture6.ppm"
capture7 = "/home/atsuro/images/ppm/capture7.ppm"
capture8 = "/home/atsuro/images/ppm/capture8.ppm"
capture9 = "/home/atsuro/images/ppm/capture9.ppm"
out_2 = "/home/atsuro/images/ppm/out-2.ppm"

getData :: FilePath -> IO [(Int, [Word8])]
getData file = do
  c <- L.readFile file
  case parse parsePGM c of
    Right gm -> return $ sep256 gm
    Left err -> error err

getPPM :: FilePath -> IO Pixmap
getPPM file = do
  c <- L.readFile file
  case parse parsePPM c of
    Right pm -> return $ pm
    Left err -> error err

initOSC :: [OSC]
initOSC = map f oscIds
  where
    f i = c_set [(freqBus i,fd+freqOffset),(panBus i,pan)]
      where
        fd = exp (log (24000*freqScale) * (i'/256))
        i' = fromIntegral (i - 20001)
        pan = if even i then f i else negate (f i)
        f j = (panScale*i'/256)

type RGBFunc
  = Int   -- ^ Y axis
 -> Word8 -- ^ Red, from 0 to 255
 -> Word8 -- ^ Green, from 0 to 255
 -> Word8 -- ^ Blue, from 0 to 255
 -> OSC

f2 :: RGBFunc
f2 i r g b =
  n_set hitId [("t_trig_"++show (oscIds!!i),1)
              ,("amp_"++show (oscIds!!i), fromIntegral (r+g+b)/255)]

applyToPixmap :: RGBFunc -> Pixmap -> [[OSC]]
applyToPixmap f a =
  map (map (\((y,x), (r,g,b)) -> f (inv y) r g b)) .
  groupBy ((==) `on` (fst . fst)) . assocs $ a
  where
    inv i = let (_,(m,_)) = bounds a in m - i

-- | Make OSC for each row of image.
mkOSC :: (Int, [Word8]) -> [OSC]
mkOSC (i,ws) = map f ws
  where
    f w =
      n_set hitId [("t_trig_"++show (oscIds!!i),1)
                  ,("amp_"++show (oscIds!!i), fromIntegral w / 255)]

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

ac6 :: UGen
ac6 = ac6' ("amp"=:0.1) ("rel"=:200e-3)
ac6' amp rel = mrg $ map mkO oscIds
  where
    mkO i = out (fromIntegral $ ampBus i) . (* amp) .
            (* ("amp_"++show i)=:1) .
            envGen kr (("t_trig_"++show i)=:0) 1 0 1 DoNothing $
            env [0,0,1,0] [0,2e-4,rel] [EnvNum (-13)] (-1) 0

test = do
  c <- L.readFile imageFile
  case parse parsePGM c of
    Right gm ->
      return $ transpose $ map (mkOSC . second (take 10)) $
      sep256 gm
    Left err -> error "Error on parsing image file"