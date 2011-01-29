{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From: defmacro.org.
--
module MetaOne where

import Data.Data (Data, Typeable, constrFields, toConstr, gmapQ)
import Data.Generics.Text (gshow)
import Data.List (isSuffixOf, nub)
import Data.Generics.Schemes (listify)
import Language.Haskell.Syntax (HsName(..))

extractWidget :: (Data a) => a -> [String]
extractWidget = nub . map (\(HsIdent a) -> a) . listify isWidget
  where
    isWidget (HsIdent actionName) = "Widget" `isSuffixOf` actionName
    isWidget _                    = False

data Foo = Foo
  { fooWidget :: String
  , fooB :: [Int] }
  deriving (Read, Show, Data, Typeable)

foo1 :: Foo
foo1 = Foo "fooOne" [1,11,111]

introspectData :: (Data a) => a -> [(String, String)]
introspectData a = zip fields (gmapQ gshow a)
  where
    fields = constrFields $ toConstr a