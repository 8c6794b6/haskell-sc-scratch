{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

Variation of Printf, take 3, extension B.
-}
module Printf03b where

import Printf03

class Rep t where
  rep :: (Format f, FormatExt01 f) => f t

instance Rep String where
  rep = eod

instance Rep r => Rep (Int -> r) where
  rep = int rep

instance Rep r => Rep (String -> r) where
  rep = str rep

instance Rep r => Rep (Double->r) where
  rep = double rep

printPair :: Rep (t1 -> String -> t2 -> String -> t) => t1 -> t2 -> t
printPair x y = printf rep "(" x ", " y ")"

printTrio ::
  Rep (t1 -> String -> t2 -> String -> t3 -> String-> t)
  => t1 -> t2 -> t3 -> t
printTrio x y z = printf rep "(" x ", " y ", " z ")"

-- ghci> printTrio "foo" (3::Int) "bar" :: String
-- "(foo, 3, bar)"
