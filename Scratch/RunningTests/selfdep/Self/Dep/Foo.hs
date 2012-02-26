{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Demo containing library function @foo@.

-}
module Self.Dep.Foo (fooSize) where

import qualified Data.Map as M

fooSize :: String -> Int
fooSize xs = M.size $ M.fromList $ zip [1..] xs
