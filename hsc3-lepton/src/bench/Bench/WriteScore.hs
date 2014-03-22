module WriteScore where

import Sound.SC3
import Sound.SC3.Lepton

main :: IO ()
main = writeScore [] n0 (toL $ ptakeT (d 300) psw) "/tmp/psw.osc"

n0 = Group 0 [Group 1 []]

d = pdouble
ds = map pdouble

psw = pappend set03 (ppar [loop01, loop02, loop03])

loop01 = psnew "rspdef1" Nothing AddToHead 1
  [("dur",  pcycle [prd 1024 (1/41)
                   ,prd 512 (2/41)
                   ,prd 256 (4/41)
                   ,prd 128 (8/41)])
  ,("freq", pmidiCPS $ pforever $ prand (pint 1) $
            (ds [40,41,48,52,55,58,62,67,70,74,79,86,90]))
  ,("pan",  pfrng (-1) 1)
  ,("atk",  pfrng 1e-4 1)
  ,("dcy",  pfrng 1e-2 1)
  ,("amp",  pfrng 1e-3 1)
  ,("n_map/fmul", pforever (d 100))]
  where
    prd x y = preplicate (pint x) (pdouble y)
    
pfrng x y = pforever (pdrange (d x) (d y))

loop02 = psnew "rspdef2" Nothing AddToHead 1
  [("dur",  pfrng 1e-1 5e-1)
  ,("freq", pexp $ pfrng (log 110) (log 11000))
  ,("atk",  pfrng 1e-4 2)
  ,("dcy",  pfrng 1e-4 2)
  ,("amp",  pfrng 1e-2 1)
  ,("pan",  pfrng (-1) 1)
  ,("q",    pfrng 1e-3 99e-2)]

loop03 = pnset 1003
  [("dur",    pfrng 4 32)
  ,("t_trig", pforever 1)]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pdouble 0.1)]
