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
module Bench.SerializePattern where

import Criterion.Main
import Data.Binary (decode, encode)
import Sound.SC3

import Scratch.Parse4 (E, e2r, e2rio, etree, tree2r)
-- import qualified Scratch.Parse5 as E5
-- import qualified Scratch.Parse7 as E7
import qualified Scratch.E as EE
import qualified Scratch.Parse8 as E8

import qualified Bench.PSPE as PSPE

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.Interpreter.Expr
import Sound.SC3.Lepton.Pattern.Interpreter.R
import Sound.SC3.Lepton.Pattern.ParseP
import Sound.SC3.Lepton.Pattern.Parse2
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Codec.Compression.QuickLZ as Q
import qualified Codec.Compression.Zlib as Z

main :: IO ()
main = do
  defaultMain $ concat 
    [ {- mkBench "pspe" pspe pspe
    , mkBench "psw" psw psw
    , -} 
    --   mkBench2 "pspe" (pspe :: E (ToOSC Double)) (pspe :: Bz (ToOSC Double)) 
    -- , mkBench2 "psw" (psw :: E (ToOSC Double)) (psw :: Bz (ToOSC Double))       
      
      mkBench3 "pspe"
      (pspe :: Bz (ToOSC Double))
      (PSPE.pspe2 :: EE.E () (ToOSC Double))
      
    , mkBench3 "psw"
      (psw :: Bz (ToOSC Double))
      (PSPE.psw2 :: EE.E () (ToOSC Double))

    ]

mkBench name pat pat' =
  [ bench ("Expr serialize " ++ name)
       (whnf (compressQ . encode . toExpr) pat)
  , bench ("Bz serialize " ++ name)
    (whnf (compressQ . lazyByteStringP) (pat'))

  , bench ("Expr deserialize " ++ name)
    (whnf (fmap toR . fromExpr . decode . decompressQ)
     (compressQ $ encode $ toExpr pat))
    
  , bench ("Bz deserialize " ++ name)
    (whnf (fmap toR . parseP . decompressQ)
     (compressQ $ lazyByteStringP $ pat'))
  ]
  
mkBench2 name pat pat' =
  [
    bench ("Bz serialize " ++ name)
    (whnf (compressQ . lazyByteStringP) (pat'))
  , bench ("Bz deserialize " ++ name)
    (whnf (parsePR . decompressQ)
     (compressQ $ lazyByteStringP $ pat'))
    
  , bench ("E serialize " ++ name) 
    (whnf (compressQ . encode . etree) pat)
  , bench ("E deserialize " ++ name) 
    (whnf (tree2r . decode . decompressQ)
     (compressQ $ encode $ etree pat))
  ]
  
mkBench3 name {- pat -} pat' pat'' =
  [
    bench ("Bz serialize " ++ name)
    (whnf (compressQ . lazyByteStringP) (pat'))
  , bench ("Bz deserialize " ++ name)
    (whnf (parsePR . decompressQ)
     (compressQ $ lazyByteStringP $ pat'))
    
  , bench ("E8 serialize " ++ name) 
    (whnf (compressQ . encode . EE.etree) pat'')
  , bench ("E8 deserialize " ++ name) 
    (whnf (E8.t2l . decode . decompressQ)
     (compressQ $ encode $ EE.etree pat''))

  ]
  
compressQ = s2l . Q.compress . l2s
decompressQ = s2l . Q.decompress . l2s

s2l :: BS.ByteString -> BSL.ByteString
s2l = BSL.fromChunks . (:[])

l2s :: BSL.ByteString -> BS.ByteString
l2s = BS.concat . BSL.toChunks

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("freq", midiCPS pspeFreq)]

-- pspeFreq =
--   pcycle
--     [prand 1
--        [pempty, plist [24,31,36,43,48,55]]
--     ,pseq (prange 2 5)
--        [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
--     ,prand (prange 3 9)
--        [74,75,77,79,81]]

pspeFreq =
  pcycle
    [prand (pval 1)
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange (pval 2) (pval 5))
       [ pval 60, prand (pval 1) [pval 63, pval 65]
       , pval 67, prand (pval 1) [pval 70,pval 72,pval 74]]
    ,prand (prange (pval 3) (pval 9))
       [pval 74,pval 75,pval 77,pval 79,pval 81]]


psw = pappend set03 (ppar [loop01, loop02, loop03])

loop01 = psnew "rspdef1" Nothing AddToHead 1
  [("dur",  pcycle [preplicate (pval 1024) (1/41)
                   ,preplicate (pval 512) (2/41)
                   ,preplicate (pval 256) (4/41)
                   ,preplicate (pval 128) (8/41)])
  ,("freq", midiCPS $ pforever $ prand (pval 1) $
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
  ,("t_trig", prepeat 1)]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pval 0.1)]
