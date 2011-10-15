{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading typefun.pdf.
From /Appendix/.

-}
module Scratch06 where

class C a where
  type F a :: *
  inj :: a -> F a
  prj :: F a -> a


{-
When we remove below comment out from type signature of bar,
GHC complain with:

> Could not deduce (F a0 ~ F a)
> from the context (C a)
>   bound by the type signature for bar :: C a => F a -> F a

Consider:

> bar (1::Int)

There's no information to help make which instance of prj to be used
in below bar.

The function bar is an instance of the infamous read-show problem, the
compositoin `show . read` which is just as ambiguous.

-}

bar x = inj (prj x)

instance C Int where
  type F Int = Int
  inj = id
  prj = id

instance C Char where
  type F Char = Int
  inj _ = 0
  prj _ = 'a'
