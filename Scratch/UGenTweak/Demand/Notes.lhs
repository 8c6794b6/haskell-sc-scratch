> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DeriveFunctor #-}

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Naive approach to make dsl for demand ugens.

> module Demand where
>
> import System.Random
> import Control.Applicative
> import Control.Monad.State
>
> import Sound.SC3
> import Sound.SC3.ID
> import Sound.SC3.Lepton

Motivation is, to make set of functions to write demand UGens easily.

* Avoid explicitly passing random seed for id character everytime.
  Wrap with newtype and pass a single seed at the time of unwrapping.

* Avoid explicitly passing mce'd value. Use list.

One of the advantages of using this DSL is, we don't need to pass
around random seed explicitly. Though, this might be disadvantage at
the same time, when one has a wish to fine control each seeds passed
to demand ugens.

Looking for another word to replace demand ....  request? claim? urge?
ask? want? supply?

... Let's use 'URGE'.

Type isgnature of 'Sound.SC3.UGen.Demand.ID.dseq' is:

   dseq :: ID i => i -> UGen -> UGen -> UGen

Its arguments are:

* ID instance for random seed
* Length of the sequence
* Values used for the sequence

What we want to do is to make a function useq with:

* Avoid passing ID instance as first argument.
* Pass list of values rather than single UGen.

hence:

    useq :: ? -> [??] -> ???

We haven't decided the type of above yet.  Since we want to use the
output of useq as input of useq, it need to be consistent:

    useq :: a -> [a] -> a

Not only for dseq, we want to use this output for other demand ugen constructing functions.

    urand  :: a -> [a] -> a
    ugeom  :: a -> a -> a
    uwhite :: a -> a -> a -> a

Sample demand UGen may look like:

> d1 :: UGen
> d1 = runDemand (useq 8 [60, 67, 74, 81, 72])
>       (mkStdGen 0x872FAC89)

At the time of writing, Jul 2011, there are 14 demand functions in hsc3.

We can make a data type for demand, and run unDemand as below:

> data Demand a
>   = Dval a
>   | Dseq (Demand a) [Demand a]
>   | Drand (Demand a) [Demand a]
>   | Dgeom (Demand a) (Demand a) (Demand a)
>   | Dwhite (Demand a) (Demand a) (Demand a)
>   deriving (Eq, Show)

> instance Functor Demand where
>   fmap f (Dval a) = Dval (f a)
>   fmap f (Dgeom len str grw) = Dgeom (fmap f len) (fmap f str) (fmap f grw)
>   fmap f (Dwhite len lo hi) = Dwhite (fmap f len) (fmap f lo) (fmap f hi)
>   fmap f _ = error "Functor instance not supported"

> instance Num (Demand UGen) where
>   (Dval a) + (Dval b) = Dval (a+b)
>   (Dval a) - (Dval b) = Dval (a-b)
>   (Dval a) * (Dval b) = Dval (a*b)
>   negate (Dval a) = Dval (negate a)
>   abs (Dval a) = Dval (abs a)
>   signum (Dval a) = Dval (signum a)
>   fromInteger = Dval . fromInteger

... and so on.

Above is an adhoc dsl. Dval is a lifting constructor that bring UGen
to our DSL.

We prepare an unwrapping function:

> unDemand :: Demand UGen -> StdGen -> (UGen, StdGen)
>
> unDemand (Dval n) g = (n,g)
>
> unDemand (Dseq len vals) g0 =
>   let (i1,g1) = random g0 :: (Int,StdGen)
>       (len',g2) = unDemand len g1
>       (vals',g3) = zipWithG vals g2
>   in  (dseq i1 len' (mce vals'), g3)
>
> unDemand (Drand len vals) g0 =
>   let (i1,g1) = random g0 :: (Int,StdGen)
>       (len',g2) = unDemand len g1
>       (vals',g3) = zipWithG vals g2
>   in  (drand i1 len' (mce vals'), g3)
>
> unDemand (Dgeom len strt grow) g0 =
>   let (i1,g1) = random g0 :: (Int,StdGen)
>       (len',g2) = unDemand len g1
>       (strt',g3) = unDemand strt g2
>       (grow',g4) = unDemand grow g3
>   in  (dgeom i1 len' strt' grow', g3)
>
> unDemand (Dwhite len lo hi) g0 =
>   let (i1,g1) = random g0 :: (Int,StdGen)
>       (len',g2) = unDemand len g1
>       (lo',g3) = unDemand lo g2
>       (hi',g4) = unDemand hi g3
>   in  (dgeom i1 len' lo' hi', g3)

> useq = Dseq
> urand = Drand
> ugeom = Dgeom
> uwhite = Dwhite

> zipWithG :: [Demand UGen] -> StdGen -> ([UGen],StdGen)
> zipWithG ds g = foldr f v ds where
>   f a (ugs,g0) = let (ug,g1) = unDemand a g0 in (ug:ugs,g1)
>   v     = ([],g)

> runDemand :: Demand UGen -> StdGen -> UGen
> runDemand d g = fst $ unDemand d g

> runDemandIO :: Demand UGen -> IO UGen
> runDemandIO = getStdRandom . unDemand

Hm,,,, could use State monad. Should we?

* dbrown length lo hi step
* dbufrd
* dbufwr
* dgeom length start grow  (x[i] = x[i-1] * grow)
* dibrown length lo hi step
* diwhite length lo hi
* drand length array
* dseq length array
* dser length array
* dseries length start step
* dstutter numRepeat input
* dswitch index array
* dswitch1 index array
* dwhite length lo hi
* dxrand length array
