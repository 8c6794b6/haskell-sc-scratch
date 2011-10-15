{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Using lam, app, z and s with pattern, take 1.
-}
module Pat01 where

import System.Random
import System.Random.Shuffle

class Pprim p where
  pval    :: a -> p h a
  pempty  :: p h a
  plist   :: [a] -> p h a
  prepeat :: a -> p h a

class Pappend p where
  pappend :: p h a -> p h a -> p h a

class Pconcat p where
  pconcat :: [p h a] -> p h a

class Prange p where
  prange :: Random a => p h a -> p h a -> p h a

class Preplicate p where
  preplicate :: p h Int -> p h a -> p h a

class Pseq p where
  pseq :: p h Int -> [p h a] -> p h a

class Pforever p where
  pforever :: p h a -> p h a

class Pcycle p where
  pcycle :: [p h a] -> p h a

-- Nouveau
class Ppair p where
  pzip :: p h a -> p h b -> p h (a,b)
  pfst :: p h (a,b) -> p h a
  psnd :: p h (a,b) -> p h b

class Plc r where
  pz   :: r (a,h) a
  ps   :: r h a -> r (any,h) a
  plam :: r (a,h) b -> r h (a->[b])
  papp :: r h (a->[b]) -> r h a -> r h b

newtype L h a = L {unL :: h -> StdGen -> [a]}

runLIO p = return . unL p () =<< newStdGen

gens :: StdGen -> [StdGen]
gens = iterate (snd . next)

instance Pprim L where
  pval a = L $ \_ _-> [a]
  pempty = L $ \_ _ -> []
  plist as = L $ \_ _ -> as
  prepeat a = L $ \_ _ -> repeat a

instance Pappend L where
  pappend p1 p2 = L $ \h g ->
    let g' = snd (next g)
    in  unL p1 h g ++ unL p2 h g'

instance Pconcat L where
  pconcat = foldr1 pappend

instance Prange L where
  prange p1 p2 = L $ \h g0 ->
    let g1 = snd (next g0)
        g2 = snd (next g1)
    in  zipWith3 (\lo hi g' -> fst (randomR (lo,hi) g'))
        (unL p1 h g0) (unL p2 h g1) (gens g2)

instance Ppair L where
  pzip p1 p2 = L $ \h g -> zip (unL p1 h g) (unL p2 h g)
  pfst p = L $ \h g -> map fst $ unL p h g
  psnd p = L $ \h g -> map snd $ unL p h g

instance Preplicate L where
  preplicate pn p = L $ \h g0 ->
    let p' = concatMap (`replicate` p) (unL pn h g0)
    in  concat $ zipWith (\q g -> unL q h g) p' (gens g0)

instance Pseq L where
  pseq pn = preplicate pn . foldr1 pappend

instance Pforever L where
  pforever p = L $ \h g -> concatMap (\g' -> unL p h g') (gens g)

instance Pcycle L where
  pcycle ps = case ps of
    [] -> pempty
    _  -> pforever (pconcat ps)

instance Plc L where
  pz         = L $ \(a,_) g -> [a]
  ps v       = L $ \(_,h) g -> unL v h g
  plam f     = L $ \h g -> repeat (\x -> unL f (x,h) g)
  papp e1 e2 = L $ \h g -> concat $ zipWith ($) (unL e1 h g) (unL e2 h g)

------------------------------------------------------------------------------
-- Tests

p01 =
  let x1 = pz; x2 = ps pz; x3 = ps (ps pz)
  in  plam (plam (plam (pseq (pval 1) [x1,x2,x3])))
      `papp` preplicate (pval 4) (prange (pval 100) (pval 200))
      `papp` plist [1002,1004,1008,1016]
      `papp` pseq (pval 1) [pval 1, plist [2,3,4], pval 5, plist [6,7,8]]

p02a =
  let x1 = pz; x2 = ps pz
  in  plam (plam (pseq (pval 1) [x1,x2]))
      `papp` plist [1,2,3,4]
      `papp` preplicate (pval 4) (prange (pval 100) (pval 200))

p02b =
  let x1 = pz; x2 = ps pz
  in plam (plam (pseq (pval 1) [x2,x1]))
     `papp` preplicate (pval 4) (prange (pval 100) (pval 200))
     `papp` plist [1,2,3,4]

p03 = plam (preplicate (prange (pval 1) (pval 8)) pz)
      `papp`
      pseq (pval 4)
      [ prange (pval 1) (pval 100)
      , plist [1,2,3]
      , pseq (prange (pval 1) (pval 4))
         [ plist [10,20,30]
         , prange (pval 1) (pval 100)
         , plist [40,50]
         , prange (pval 1) (pval 100)
         ]
      ]

p04 :: L () (Double,(Double,Double))
p04 =
  pzip
  (preplicate (pval 2) (plist [1..10]))
  (pzip
   (pcycle [plist [1,2,3,4], prange (pval 5) (pval 10)])
   (pforever (prange (pval 5) (pval 10))))

p05 :: L () Double
p05 =
  let x = pfst p04; y = pfst (psnd p04); z = psnd (psnd p04)
  in  pconcat [x,y,z]

p06 =
  (plam (let x = pfst pz; y = pfst (psnd pz); z = psnd (psnd pz)
         in  pconcat [x,y,z]))
  `papp` p04
