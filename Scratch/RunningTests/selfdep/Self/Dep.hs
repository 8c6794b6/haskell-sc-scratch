{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Wrapper module for re-export.

-}
module Self.Dep
  ( module Self.Dep.Foo
  , module Self.Dep.Bar
  ) where

import Self.Dep.Foo
import Self.Dep.Bar
