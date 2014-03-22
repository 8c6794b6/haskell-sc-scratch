module Bench.Expr where

import Control.DeepSeq
import Data.Binary (encode, decode)
import Sound.SC3
import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.Interpreter.Expr
import Sound.SC3.Lepton.Pattern.Interpreter.R
import Sound.SC3.Lepton.Pattern.ParseP

main :: IO ()
main =
  let p = pconcat (replicate 1000 psw)
      e = fmap toExpr . fromExpr . decode . encode . toExpr $ p
  in  e `deepseq` putStrLn "done"

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
