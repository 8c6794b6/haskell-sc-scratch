------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with comb filter.
--
module Jos.Filters.Comb where

import Data.Maybe (catMaybes)

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

w = withSC3

setupComb fd = do
  mapM_ (\(n,u) -> loadSynthdef n u fd) 
    [("noise",noise),("cmb001",cmb001),("cmb002",cmb002)]

t001 = 
  Group 1
    [Synth 1000 "noise" []
    ,Synth 1001 "cmb002" ["input":=0]]
    
hit nid = w $ flip send $ n_set nid [("t_trig",1)]
    
noise :: UGen
noise = out 0 $ mce [n,n]
  where 
    n = whiteNoise 'a' ar * 0.1

-- | Example filter:
--    
-- > > y(n) = x(n) + (0.5^3) * x(n-3) - (0.9^5) * y(n-5)
--    
cmb001 :: UGen
cmb001 = replaceOut i $ mce [sig, sig]
  where
    sig = sig' + (b*sig'')
    sig' = nz + (a*nz')
    sig'' = iterate delay1 sig' !! 5
    nz' = iterate delay1 nz !! 3
    nz = in' 2 ar i
    a = (0.5^3)
    b = (0.9^5)
    i = "input"@@0
    
cmb002 :: UGen
cmb002 = replaceOut i $ mce [y, y]    
  where
    y = filt x [1,0,0,0,0,0.9^5]
    x = filt n [1,0,0,0.5^3]
    n = in' 2 ar i
    i = "input"@@0
    
filt :: Num a => UGen -> [a] -> UGen
filt source coefs = sum $ catMaybes $ foldr f v coefs'
  where
    v = []    
    f (n,g) bs 
      | g == 0    = Nothing : bs
      | otherwise = Just (iterate delay1 source !! n) : bs
    coefs' = zip [1..] coefs
