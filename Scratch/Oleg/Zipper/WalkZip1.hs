{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
-- * http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WalkZip1/
--
module Zipper.WalkZip1 where

import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Writer (WriterT(runWriterT), tell)
import Data.Generics (Typeable, Data, gmapM, mkM)
import Data.Monoid (Any(Any))
import Data.Maybe (isJust, fromMaybe)

data Term = V String
          | L String Term
          | A Term Term
          deriving (Eq, Read, Show, Typeable, Data)

term :: Term
term = A t t where
  t = L "x" (A (V "x") (V "x"))

type Walk from to part whole = forall m. (Monad m)
  => (from -> part -> m (Maybe part, to)) -> whole -> m (Maybe whole)

data After = After
           deriving (Eq, Ord, Read, Show)

class (Eq to) => To to where
  after :: to

instance To After where
  after = After

data Before = Before
            deriving (Eq, Ord, Read, Show)

class (Eq from) => From from where
  before :: from

instance From Before where
  before = Before

stop :: from -> Walk from After a a
stop from visit = liftM fst . visit from
-- stop from = \visit part -> liftM fst (visit from part)

class (From from, To to, Show from, Read to) => Next from to where
  next :: from -> to

instance Next Before After where
  next Before = After

tourist :: (Next from to, Show part) => from -> part -> IO (Maybe part, to)
tourist from part = do
  putStrLn $ show from ++ ": " ++ show part
  return (Nothing, next from)

tourist' :: (Next from to) => from -> Term -> IO (Maybe Term, to)
tourist' from part = do
  putStrLn $ show from ++ ": " ++ show part
  return (Just (V "poof"), next from)

gwalk :: (Typeable a, Data b) => Walk from to part a -> Walk from to part b
gwalk walk visit a = do
  (a', Any dirty) <- runWriterT (gmapM (mkM f) a)
  return $ scavenge dirty a'
  where
    f part = do
      part' <- lift $ walk visit part
      tell $ Any $ isJust part'
      return $ fromMaybe part part'

scavenge :: Bool -> a -> Maybe a
scavenge True a  = Just a
scavenge False _ = Nothing
