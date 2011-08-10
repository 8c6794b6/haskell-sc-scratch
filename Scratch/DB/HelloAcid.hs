{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example acid-state with HelloIxSet.

-}
module HelloAcid where

import Data.Data

import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.IxSet
import Data.SafeCopy

import HelloIxSet

data EntryIxSet = EntryIxSet (IxSet Entry)
  deriving (Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Id)
$(deriveSafeCopy 0 'base ''Author)
$(deriveSafeCopy 0 'base ''Updated)
$(deriveSafeCopy 0 'base ''Content)
$(deriveSafeCopy 0 'base ''Entry)
$(deriveSafeCopy 0 'base ''EntryIxSet)

saveEntries :: IxSet Entry -> Update EntryIxSet ()
saveEntries ixs = put (EntryIxSet ixs)

loadEntries :: Query EntryIxSet EntryIxSet
loadEntries = ask

$(makeAcidic ''EntryIxSet ['saveEntries, 'loadEntries])