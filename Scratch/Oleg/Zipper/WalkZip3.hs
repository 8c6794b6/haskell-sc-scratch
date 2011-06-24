{-# LANGUAGE FlexibleInstances #-}
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
module Zipper.WalkZip3 where

import Zipper.WalkZip1
import Zipper.WalkZip2
import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Cont (ContT(ContT), runContT)
import Control.Monad.Error (Error(noMsg), ErrorT(runErrorT), throwError)
import Data.Maybe (isJust, fromMaybe)
import Data.Generics (Data)
import Text.Show.Functions ()

data Zipper from to part a
  = Done a
  | Stop from part (Maybe part -> to -> Zipper from to part a)
  deriving (Show)

instance Monad (Zipper from to part) where
  return = Done
  Done a           >>= k = k a
  Stop from part c >>= k = Stop from part c'
    where c' part' to = c part' to >>= k

zipper :: Walk from to part whole -> whole
       -> Zipper from to part (Maybe whole)
zipper walk whole = runContT (walk visit whole) return where
  visit from part = ContT (Stop from part . curry)

start :: Zipper (Exit Before) (Enter After) Term (Maybe Term)
start = zipper (throughout (stop Before)) term

continue :: Zipper from to part whole -> Maybe part -> to
         -> Zipper from to part whole
continue (Done _) = error "Zipper is done, not at a stop"
continue (Stop _ _ c) = c

data Diff part to = Diff (Maybe part) to (Diff part to)

instance (Show part, Show to) => Show (Diff part to) where
  showsPrec = loop 3 where
    loop 0 _ _ = showString "..."
    loop l d (Diff part to diff) = showParen (d > 0) $
      showString "Diff " . showsPrec 11 part . showChar ' ' .
      showsPrec 11 to . showString " $ " . loop (l-1) 0 diff

same :: (To to) => Diff part to
same = Diff Nothing after same

replay :: Zipper from to part whole -> Diff part to -> whole
replay (Done whole) _ = whole
replay (Stop _ _ c) (Diff part to diff) = replay (c part to) diff

walkDiff :: (To to) => Zipper from to part whole
         -> Walk (Exit (Zipper from to part whole))
                 (Enter After) (Diff part to) (Diff part to)
walkDiff zipper = stop zipper `around`
  \visit ~(Diff part to diff) -> case zipper of
    Done _ -> return Nothing
    Stop _ _ c -> liftM (liftM (Diff part to))
                  (walkDiff (c part to) visit diff)

data BackForth to = Back | Forth to
                  deriving (Eq, Ord, Read, Show)

instance (To to) => To (BackForth to) where
  after = Forth after

instance (Next from to) => Next from (BackForth to) where
  next = Forth . next

backForth :: (To to) => Walk from to part whole
          -> Walk from (BackForth to) part whole
backForth walk visit whole =
  liftM (either id (>>= replay za))
  (runErrorT (walkDiff za visit' same))
  where
    za = zipper walk whole
    visit' (Exit _ (Done whole)) _ = throwError whole
    visit' (Exit _ (Stop from part _)) (Diff partD toD diffD) = do
      (part, to) <- lift (visit from (fromMaybe part partD))
      let diff' | isJust part = Just (Diff part toD diffD)
                | otherwise   = Nothing
      return $ case to of
        Back -> (diff', after)
        Forth to -> (if to == toD then diff'
                       else Just (Diff part to same), Enter)

instance Error (Maybe whole) where
  noMsg = Nothing

walkTerm :: Walk Before After Term Term
walkTerm = gwalk (stop Before)

through :: (Data a) => Walk (Exit Before) (Enter (BackForth After)) a a
through = stop' `around` throughout (backForth (gwalk stop'))
  where stop' visit = liftM fst . visit before