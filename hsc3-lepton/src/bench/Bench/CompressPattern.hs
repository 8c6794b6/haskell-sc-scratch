{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Bench mark for pattern serialization and deserialization.
-}

module CompressPattern where

import Criterion.Main
import Data.Binary (decode, encode)
import Sound.SC3
import Sound.SC3.Lepton

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- Tried these two.
--
-- BZip compress to small size, but slow.
-- Gzip was show almost same size and performance with zlib.
--
-- import qualified Codec.Compression.BZip as B
-- import qualified Codec.Compression.GZip as G

import qualified Codec.Compression.QuickLZ as Q
import qualified Codec.Compression.Snappy.Lazy as S
import qualified Codec.Compression.Zlib as Z

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- import Scratch.SampleData
-- import Scratch.ChuckOTF

main :: IO ()
main = do
  showSizes "hatoP" hatoP
  showSizes "pspe" pspe
  showSizes "psw" psw
  defaultMain $ concat
    [ mkBenches "hatoP" hatoP 
    , mkBenches "pspe" pspe
    , mkBenches "psw" psw
    ]

showSizes name p = do
  putStrLn $ "ByteString length of " ++ name
  let p' = lazyByteStringP p
  mapM_ putStrLn
    [ "Size without compression: " ++ show (BSL.length p')
    , "Size after compressed with Zlib: " ++ show (BSL.length $ compressZ p')
    , "Size after compressed with QuickLZ: " ++ 
      show (BSL.length $ s2l $ Q.compress $ byteStringP p)
    , "Size after compressed with Snappy: " ++ show (BSL.length $ S.compress p')
    ]

mkBenches name p =
  [ --------------------------------------------------------------------------
    -- Compress

    bench ("Bz serialize " ++ name ++ ", no compression")
    (whnf (lazyByteStringP) p)

  , bench ("Bz serialize " ++ name ++ ", compression with Zlib")
    (whnf (compressZ . lazyByteStringP) p)

  , bench ("Bz serialize " ++ name ++ ", compression with QuickLZ")
    (whnf (s2l . Q.compress . byteStringP) p)

  , bench ("Bz serialize " ++ name ++ ", compression with Snappy")
    (whnf (S.compress . lazyByteStringP) p)

    --------------------------------------------------------------------------
    -- Decompress

  , bench ("Bz deserialize " ++ name ++ ", no decompression")
    (whnf (fmap toR . parseP) (lazyByteStringP p))

  , bench ("Bz deserialize " ++ name ++ ", decompression with Zlib")
    (whnf (fmap toR . parseP . Z.decompress)
     (compressZ . lazyByteStringP $ p))
    
  , bench ("Bz deserialize " ++ name ++ ", decompression with QuickLZ")
    (whnf (fmap toR . parseP . s2l . Q.decompress . l2s)
     (s2l . Q.compress . byteStringP $ p))

  , bench ("Bz deserialize " ++ name ++ ", decompression with Snappy")
    (whnf (fmap toR . parseP . S.decompress)
     (S.compress . lazyByteStringP $ p))
  ]

s2l :: BS.ByteString -> BSL.ByteString
s2l = BSL.fromChunks . (:[])

l2s :: BSL.ByteString -> BS.ByteString
l2s = BS.concat . BSL.toChunks 

compressZ = 
  Z.compressWith (Z.defaultCompressParams { Z.compressLevel = Z.bestSpeed })

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("freq", midiCPS pspeFreq)]

pspeFreq =
  pcycle
    [prand 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange 2 5)
       [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
    ,prand (prange 3 9)
       [74,75,77,79,81]]

psw = pappend set03 (ppar [loop01, loop02, loop03])

loop01 = psnew "rspdef1" Nothing AddToHead 1
  [("dur",  pcycle [preplicate 1024 (1/41)
                   ,preplicate 512 (2/41)
                   ,preplicate 256 (4/41)
                   ,preplicate 128 (8/41)])
  ,("freq", midiCPS $ pforever $ prand 1 $
            [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ prange (-1) 1)
  ,("atk",  pforever $ prange 1e-4 1)
  ,("dcy",  pforever $ prange 1e-2 1)
  ,("amp",  pforever $ prange 1e-3 1)
  ,("n_map/fmul", pforever 100)]

loop02 = psnew "rspdef2" Nothing AddToHead 1
  [("dur",  pforever $ prange 1e-1 5e-1)
  ,("freq", pforever $ exp $ prange (log 110) (log 11000))
  ,("atk",  pforever $ prange 1e-4 2)
  ,("dcy",  pforever $ prange 1e-4 2)
  ,("amp",  pforever $ prange 1e-2 1)
  ,("pan",  pforever $ prange (-1) 1)
  ,("q",    pforever $ prange 1e-3 99e-2)]

loop03 = pnset 1003
  [("dur",    pforever $ prange 4 32)
  ,("t_trig", pforever 1)]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pval 0.1)]

t = 0.5

hatoP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever t)
  ,("bnum", prepeat 3)
  ,("pan", prepeat (-0.3))
  ,("amp", pforever (prange 0.2 0.4))]
