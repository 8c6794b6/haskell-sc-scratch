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
-- Using pgm image as input data for oscillators. 
-- Input image file width need to be 256.
--
module Sound.Study.ForAPileOfOscillators.A006 where

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Word (Word8)
import Data.List (transpose)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8

import Data.PGM.Parse
import Data.PGM.Types
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.Study.ForAPileOfOscillators.Common

main :: IO ()
main = print "No GUI for this piece"

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
    threadDelay $ floor $ durScale * 1e6

writeA006Score :: FilePath -> IO ()
writeA006Score path = do
  dat <- getData imageFile
  let os = zipWith f [1..] (transpose $ map mkOSC dat)
      f t ms = map (Bundle (NTPr (t*durScale)) . (:[])) ms
      ini = map (Bundle (NTPr 0) . (:[])) $ (treeToNew 0 a006Nodes ++ initOSC)
  writeNRT path $ ini ++ concat os

a006Nodes :: SCNode
a006Nodes =
  grp 1
    [grp 10 []
    ,grp 11 [syn hitId "ac6" ["amp":=ampScale,"rel":=0.8*durScale]]
    ,grp 12 oscs]

grp = Group
syn = Synth
hitId = 1100

timeScale = 0.03
durScale = 0.03
ampScale = 0.05
freqOffset = 50
freqScale = 0.895
panScale = 0.35

imageFile :: FilePath
imageFile = capture3

-- imageFile = "/home/atsuro/images/pgm/out2.pgm" -- 256x256
-- imageFile = "/home/atsuro/images/pgm/out3.pgm" -- 256x256
-- imageFile = "/home/atsuro/images/pgm/out4.pgm" -- 256x256

uniitiled_1 = "/home/atsuro/images/pgm/Untitled.pgm"  -- 640x256
untitiled_5  = "/home/atsuro/images/pgm/untitled5.pgm" -- 1024x256
untitiled_52 = "/home/atsuro/images/pgm/untitled5_2.pgm" -- 1024x256
mandelbrot256 = "/home/atsuro/images/pgm/mandelbrot_256.pgm" -- 256x256
mandelbrot1024 = "/home/atsuro/images/pgm/mandelbrot_1024x256.pgm" -- 1024x256
lenna = "/home/atsuro/images/pgm/lenna_1024x256.pgm" -- 1024x256
-- imageFile = "/home/atsuro/images/pgm/a.pgm" -- 800x256
capture2 = "/home/atsuro/images/pgm/capture2.pgm" -- 800x256
capture3 = "/home/atsuro/images/pgm/capture3.pgm" -- 701x256
bsd = "/home/atsuro/images/pgm/bsd.pgm" -- 2048x256

getData :: FilePath -> IO [(Int, [Word8])]
getData file = do
  c <- L.readFile file
  case parse parsePGM c of
    Right gm -> return $ sep256 gm
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

a006Func :: Int -- ^ Y axis
         -> Int -- ^ Red, from 0 to 255
         -> Int -- ^ Green, from 0 to 255
         -> Int -- ^ Blue, from 0 to 255
         -> OSC
a006Func idx r g b = undefined

-- | Make OSC for each row of image. OSC is list of bundled message.
-- mkOSC :: (Int,[Word8]) -- ^ (Index in list, grey scale values)
--       -> [OSC]
mkOSC (i,ws) = map f ws
  where
    f w =
      n_set hitId [("t_trig_"++show (oscIds!!i),fromIntegral w {- 1 -})]

-- | Extract, repack, and convert values from pgm image.
--         
-- Each [Word8] result contains list of values between 0 to 1.
--    
sep256 :: Greymap -> [(Int,[Word8])]
sep256 gm = go 256 (greyData gm)
  where
    go n bs
      | L.null bs = []
      | otherwise = (n,L.unpack (L.map f cs)):go (pred n) rest
      where 
        (cs,rest) = L.splitAt (fromIntegral $ greyWidth gm) bs
    f x | fromIntegral x <= (greyMax gm) `div` 2 = 1
        | otherwise                              = 0

ac6 :: UGen
ac6 = ac6' ("amp"=:0.1) ("rel"=:200e-3)
ac6' amp rel = mrg $ map mkO oscIds
  where
    mkO i = out (fromIntegral $ ampBus i) . (* amp) .
            envGen kr (("t_trig_"++show i)=:0) 1 0 1 DoNothing $
            env [0,0,1,0] [0,2e-4,rel] [EnvNum (-13)] (-1) 0

test = do
  c <- L.readFile imageFile
  case parse parsePGM c of
    Right gm ->
      return $ transpose $ map (mkOSC . second (take 10)) $
      sep256 gm
    Left err -> error "Error on parsing image file"