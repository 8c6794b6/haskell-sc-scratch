{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable (NoMonomorphismRestriction)

Pattern expression in debruijn, take 1.
-}
module Pdebruijn where

import System.Random

{-
Goal is, sharing data between patterns.

In TTFdB example, Symantics class has

> class Symantics repr where
>   int :: Int -> repr h Int
>   add :: repr h Int -> repr h Int -> repr h Int
>   z   :: repr (a,h) a
>   s   :: repr h a -> repr (any,h) a
>   lam :: repr (a,h) b -> repr h (a->b)
>   app :: repr h (a->b) -> repr h a -> repr h b

lam, app, z could be used like:

> td3 :: Symantics repr => repr h ((Int -> Int) -> Int)
> td3 = lam (add (app z (int 1)) (int 2))

-}

class Pval p where
  pval :: Show a => a -> p h a

class Pint p where
  pint :: Int -> p h Int

class Plist p where
  plist :: Show a => [a] -> p h a

class Pempty p where
  pempty :: p h a

class Prepeat p where
  prepeat :: Show a => a -> p h a

class Pappend p where
  pappend :: p h a -> p h a -> p h a

class Preplicate p where
  preplicate :: p h Int -> p h a -> p h a

class Pseq p where
  pseq :: p h Int -> [p h a] -> p h a

class Prand p where
  prand :: p h Int -> [p h a] -> p h a

class Prange p where
  prange :: Random a => p h a -> p h a -> p h a

------------------------------------------------------------------------------
-- Debruijn

{-

For definint instance of Plam, we want a function with its signature as:

> trans :: (a->[b]) -> [a->b]

So that:

> instance Plam R where
>   plam p = R $ \g h ->
>     let p' = \x -> unR p g (x,h)
>     in  trans p'

became possible.

How can we write 'trans'? Or is there other way?

Alternatively, modify Plam and Papp to:

> class Plam p where
>   plam :: p (a,h) b -> p h (a->[b])
>
> class Papp p where
>   papp :: p h (a->[b]) -> p h a -> p h b

-}

class Pz p where
  pz :: p (a,h) a

class Ps p where
  ps :: p h a -> p (any,h) a

class Plam p where
  plam :: p (a,h) b -> p h (a->[b])

class Papp p where
  papp :: p h (a->[b]) -> p h a -> p h b

------------------------------------------------------------------------------
-- R

newtype R h a = R {unR :: StdGen -> h -> [a]}

type Env = ()

evalPIO :: R () a -> IO [a]
evalPIO p = newStdGen >>= \g -> return $ unR p g ()

gens :: RandomGen a => a -> [a]
gens = iterate (snd . next)

instance Pval R    where pval a = R $ \_ _ -> [a]
instance Plist R   where plist as = R $ \_ _ -> as
instance Pempty R  where pempty = R $ \_ _ -> []
instance Prepeat R where prepeat a = R $ \_ _ -> repeat a

instance Pappend R where
  pappend p1 p2 = R $ \g h -> unR p1 g h ++ unR p2 g h

instance Preplicate R where
  preplicate pn p = R $ \g h ->
    let p' = concatMap (`replicate` p) (unR pn g h)
    in  concat $ zipWith3 unR p' (gens g) (repeat h)

instance Pseq R where pseq n = preplicate n . foldr1 pappend

instance Prange R where
  prange pl ph = R $ \g0 h ->
    let g1 = snd $ next g0
        g2 = snd $ next g1
        f lo hi g' = fst (randomR (lo,hi) g')
    in  zipWith3 f (unR pl g0 h) (unR ph g1 h) (gens g2)

instance Pz R where pz = R $ \_ (a,_) -> [a]

instance Ps R where ps p = R $ \g (_,h) -> unR p g h

instance Plam R where
  plam p = R $ \g h -> repeat $ \x -> unR p g (x,h)

instance Papp R where
  papp pf pa = R $ \g h -> concat $ zipWith ($) (unR pf g h) (unR pa g h)

pe1 = plam (plam (pseq (ps pz) [pval 1, pz]))
pe1' = papp (papp pe1 (pval 3)) (plist [2,3,4])
pe2 = pseq (pval 3) [pval 1, plist [2,3,4]]


------------------------------------------------------------------------------
-- S

newtype S h a = S {unS :: Int -> String}

viewP e = unS e 0

showPs ps h = case ps of
  []     -> ""
  (x:xs) -> '[' : unS x h ++ showPs' xs h
  where
    showPs' xs' h' = case xs' of
      []     -> "]"
      (y:ys) -> ',' : unS y h ++ showPs' ys h'

s1s2 name = \p1 p2 ->
  S $ \h -> name ++ " (" ++ unS p1 h ++ ") (" ++ unS p2 h ++ ")"

s1ss name = \pn ps ->
  S $ \h -> name ++ " (" ++ unS pn h ++ ") " ++ showPs ps h

instance Pval S where pval a = S $ \_ -> "pval " ++ show a
instance Plist S where plist as = S $ \_ -> "plist " ++ showList as ""
instance Pempty S where pempty = S $ \_ -> "pempty"
instance Prepeat S where prepeat a = S $ \_ -> "prepeat " ++ show a
instance Pappend S where pappend = s1s2 "pappend"
instance Preplicate S where preplicate = s1s2 "preplicate"
instance Pseq S where pseq = s1ss "pseq"
instance Prand S where prand = s1ss "prand"

instance Pz S where pz = S $ \h -> "x" ++ show (h-1)
instance Ps S where ps e = S $ \h -> unS e (h-1)

instance Plam S where
  plam e = S $ \h ->
    let x = "x" ++ show h in "(\\" ++ x ++ " -> " ++ unS e (h+1) ++ ")"
instance Papp S where
  papp e1 e2 = S $ \h -> '(' : unS e1 h ++ " " ++ unS e2 h ++ ")"