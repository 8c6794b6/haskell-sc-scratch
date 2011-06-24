{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- * http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WalkZip2/
--
module Zipper.WalkZip2 where

import Zipper.WalkZip1
import Data.Generics (Data)
import Data.Char (isSpace)

data Exit from = Exit Bool from
               deriving (Eq, Ord, Read, Show)

data Enter to = Enter | To to
              deriving (Eq, Ord, Read, Show)

instance (From from) => From (Exit from) where
  before = Exit False before

instance (To to) => To (Enter to) where
  after = To after

instance (Next Before to) => Next (Exit Before) (Enter to) where
  next (Exit False Before) = Enter
  next (Exit True  Before) = To after

around :: Walk from to part whole
       -> Walk (Exit from) (Enter to) part part
       -> Walk (Exit from) (Enter to) part whole
around walkOuter walkInner visit = walkOuter (visit' False False) where
  visit' dirty around from part = do
    (part1_, to) <- visit (Exit around from) part
    let (dirty1, part1) = pollute dirty part part1_
    case to of
      Enter -> do
        part2_ <- walkInner visit part1
        let (dirty2, part2) = pollute dirty1 part1 part2_
        visit' dirty2 True from part2
      To to -> return (scavenge dirty1 part1, to)

pollute :: Bool -> a -> Maybe a -> (Bool, a)
pollute dirty a Nothing = (dirty, a)
pollute _ _ (Just a)    = (True, a)

newYork :: Walk from to part whole
newYork _ _ = return Nothing

throughout :: (Data a) => Walk from to a a -> Walk (Exit from) (Enter to) a a
throughout level = level `around` gwalk (throughout level)

keyboard :: (Next from to, Show a, Read a) => from -> a -> IO (Maybe a, to)
keyboard from x = do
  putStr (show from ++ ": " ++ show x ++ "\n? ")
  line <- getLine
  return (if all isSpace line
             then (Nothing, next from)
             else read line)