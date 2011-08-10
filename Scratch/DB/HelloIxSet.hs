{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Example in haddock of Data.IxSet.

-}
module HelloIxSet where

import Foreign.C.Types
import Data.Data
import Data.Int
import Data.Time
import Data.Time.Clock.POSIX

import Data.IxSet

data Entry = Entry
  { eAuthor :: Author
  , eJointAuthor :: [Author]
  , eUpdated :: Updated
  , eId :: Id
  , eContent :: Content
  } deriving (Eq,Show,Ord,Data,Typeable)

newtype Updated = Updated Int64
  deriving (Eq,Show,Ord,Data,Typeable,Num)

newtype Id = Id Int64
  deriving (Eq,Show,Ord,Num,Data,Typeable)

newtype Content = Content String
  deriving (Eq,Show,Ord,Data,Typeable)

newtype Author = Author Email
  deriving (Eq,Show,Ord,Data,Typeable)

type Email = String

getWords :: Entry -> [Word]
getWords (Entry _ _ _ _ (Content s)) = map Word $ words s

newtype Word = Word String
  deriving (Eq,Show,Ord,Data,Typeable)

instance Indexable Entry where
  empty = ixSet
    [ ixFun (return . eAuthor)
    , ixFun (return . eId)
    , ixFun (return . eUpdated)
    , ixFun getWords
    ]

e1,e2,e3,e4,e5 :: Entry

e1 = Entry (Author "john@doe.com")  [] 1312894283 1
     (Content "blah blah blah")

e2 = Entry (Author "john@doe.com")  [] 1312894284 2
     (Content "foo foo foo foo foo")

e3 = Entry (Author "foo@bar.com")   [] 1312904323 3
     (Content "foo bar buzz buzz buzz")

e4 = Entry (Author "buzz@buzz.com") [] 1312942443 4
     (Content "skdsjdlkjslkdjfsldfj")

e5 = Entry (Author "buzz@buzz.com") [] 1313088338 5
     (Content "buzz buzz buzz buzz buzz")

entries :: IxSet Entry
entries = foldr insert empty [e1,e2,e3,e4,e5]

dumpEntries :: (Show a, Ord a) => IxSet a -> IO ()
dumpEntries = mapM_ print . toList
