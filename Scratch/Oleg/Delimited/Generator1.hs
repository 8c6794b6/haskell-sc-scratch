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
-- * http://okmij.org/ftp/continuations/CCmonad/Generator1.hs
--
module Delimited.Generator1 where

import Delimited.CCExc
import Control.Monad.Trans (liftIO, lift)

import Control.Monad.ST
import Data.STRef

type Label = Int
data Tree = Leaf
          | Node Label Tree Tree
          deriving (Show)

make_full_tree :: Int -> Tree
make_full_tree depth = loop 1 depth where
  loop label 0 = Leaf
  loop label n = Node label
                   (loop (2*label) (pred n))
                   (loop (2*label+1) (pred n))

tree1 = make_full_tree 3

type P m a = PS (Res m a)

newtype Res m a = Res ((a -> CC (P m a) m ()) -> CC (P m a) m ())

outRes :: (a -> CC (P m a) m ()) -> Res m a -> CC (P m a) m ()
outRes body (Res f) = f body

yield :: Monad m => a -> CC (P m a) m ()
yield v = shift0P ps (\k -> return . Res $ \b -> b v >> k () >>= outRes b)

enumerate :: Monad m =>
             CC (P m a1) m a -> (a1 -> CC (P m a1) m ()) -> CC (P m a1) m ()
enumerate iterator body =
  pushPrompt ps (iterator >> (return . Res . const $ return ())) >>=
    outRes body

in_order :: Monad m => Tree -> CC (P m Label) m ()
in_order t = case t of
  Leaf                  -> return ()
  Node label left right -> do
    in_order left
    yield label
    in_order right

test_io :: IO ()
test_io = runCC $ enumerate (in_order tree1) (liftIO . print)

test_st :: [Label]
test_st = runST $ do
  res <- newSTRef []
  let body v = modifySTRef res (v:)
  runCC $ enumerate (in_order tree1) (lift . body)
  readSTRef res >>= return . reverse