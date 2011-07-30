{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable
-}
module Sound.Study.ForNoisesAndFilters.B002.Node where

import Sound.SC3
import Sound.SC3.Lepton hiding (s)
import Sound.Study.ForNoisesAndFilters.B002.Synthdef

go :: IO ()
go = withSC3 $ patchNode n0

n0 :: SCNode
n0 =
  g 0
  [g 1
   [s 1000 "pp01"
    ["out":=10]
   ,s 1001 "fshift"
    ["out":=0,"a_in":<=10]
   ,s 1002 "fshift"
    ["out":=1,"a_in":<=11]]]

g :: NodeId -> [SCNode] -> SCNode
g = Group

s :: NodeId -> SynthName -> [SynthParam] -> SCNode
s = Synth
