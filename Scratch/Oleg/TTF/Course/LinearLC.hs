{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/tagless-final/course/LinearLC.hs>

-}
module LinearLC where

newtype F a = F a
data U = Used

class LSymantics repr where
  int :: Int -> repr hi hi Int
  add :: repr hi h Int -> repr h ho Int -> repr hi ho Int
  
  z   :: repr (F a,h) (U,h) a
  s   :: repr hi ho a -> repr (any,hi) (any,ho) a
  app :: repr hi h (a->b) -> repr h ho a -> repr hi ho b
  
class LinearL repr hi ho where  
  lam :: repr (F a,hi) (U,ho) b -> repr hi ho (a->b)
  
tl1 = add (int 1) (int 2)

-- Cannot write this.
-- tl2 = lam (add z z)
tl2o = lam (add z (s z))

tl3 = lam (add (app z (int 1)) (int 2))

tl4 = lam (lam (add z (s z)))

tl5 = lam (app (lam z) z)

newtype R hi ho a = R {unR :: hi -> (a,ho)}

instance LSymantics R where
  int x = R $ \hi -> (x,hi)
  -- add e1 e2 = 