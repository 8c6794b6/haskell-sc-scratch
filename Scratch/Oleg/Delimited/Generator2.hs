{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
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
-- * http://okmij.org/ftp/continuations/CCmonad/Generator2.hs
--
module Delimited.Generator2 where

-- import Delimited.CCExc
import Delimited.CCCxe
import Control.Monad.Trans (liftIO, lift)
import Data.Typeable

type Label = Int
data Tree = Leaf
          | Node Label Tree Tree
          deriving (Eq, Show)

make_full_tree :: Int -> Tree
make_full_tree depth = f 1 depth where
  f label 0 = Leaf
  f label n = Node label (f (2*label) (pred n)) (f (2*label+1) (pred n))

tree1 = make_full_tree 3

-- Originally from:
--
-- * http://en.literateprograms.org/Binary_search_tree_%28Haskell%29
--
pict_tree :: Tree -> IO ()
pict_tree t = putStr (pic "" t ++ "\n") where
  pic ind Leaf         = ind
  pic ind (Node x l r) = pic ('\t':ind) r ++ "\n" ++
                         ind ++ show x ++
                         pic ('\t':ind) l

newtype ResP m a = ResP ((a -> CC PP m ()) ->  CC PP m ())

instance Typeable1 m => Typeable1 (ResP m) where
  typeOf1 x = mkTyConApp (mkTyCon "ResP") [m]
    where m = typeOf1 (undefined :: m ())

outResP :: (b -> CC PP a ()) -> ResP a b -> CC PP a ()
outResP body (ResP f) = f body

ppy :: (Typeable1 m, Typeable a) => Prompt PP m (ResP m a)
ppy = pp

yieldP :: (Typeable1 m, Typeable a, Monad m) => a -> CC PP m ()
yieldP v = shift0P ppy (\k -> return . ResP $ \b -> b v >> k () >>= outResP b)

enumerateP :: (Typeable1 m, Typeable a, Monad m) =>
  CC PP m () -> (a -> CC PP m ()) -> CC PP m ()
enumerateP iterator body =
  pushPrompt ppy (iterator >> (return . ResP . const $ return ())) >>=
  outResP body

in_orderP :: (Typeable1 m, Monad m) => Tree -> CC PP m ()
in_orderP Leaf = return ()
in_orderP (Node x l r) = do
  in_orderP l
  yieldP x
  in_orderP r

test_ioP :: IO ()
test_ioP = runCC $
  enumerateP (in_orderP tree1) (liftIO . (print :: Int -> IO ()))

newtype Res m a = Res ((a -> CC (PM a) m ()) -> CC (PM a) m ())

instance Typeable1 m => Typeable1 (Res m) where
  typeOf1 x = mkTyConApp (mkTyCon "Res") [m]
    where m = typeOf1 (undefined :: m ())

outRes :: (p -> CC (PM p) a ()) -> Res a p -> CC (PM p) a ()
outRes body (Res f) = f body

py :: (Typeable1 m, Typeable a) => Prompt (PM a) m (Res m a)
py = pm

yield :: (Typeable1 m, Typeable a) => Monad m => a -> CC (PM a) m ()
yield v = shift0P py (\k -> return . Res $ \b -> b v >> k () >>= outRes b)

enumerate :: (Typeable1 m, Typeable a, Monad m) =>
  CC (PM a) m () -> (a -> CC (PM a) m ()) -> CC (PM a) m ()
enumerate iterator body =
  pushPrompt py (iterator >> (return . Res . const $ return ())) >>=
  outRes body

in_order :: (Typeable1 m, Monad m) => Tree -> CC (PM Label) m ()
in_order Leaf = return ()
in_order (Node label left right) = do
  in_order left
  yield label
  in_order right

test_io :: IO ()
test_io = runCC $ enumerate (in_order tree1) (liftIO . print)

newtype Acc a = Acc [a] deriving (Eq, Show, Typeable)
toAcc v (Acc l) = return . Acc $ v:l

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show, Typeable)

instance Monad Identity where
  return = Identity
  m >>= f = f $ runIdentity m

pa :: (Typeable a) => Prompt (PM a) m (Acc a)
pa = pm

acc :: (Typeable a, Monad m) => a -> CC (PM a) m ()
acc v = shift0P pa (\k -> k () >>= toAcc v)

accumulated :: (Typeable a, Monad m) => CC (PM a) m () -> CC (PM a) m [a]
accumulated body =
  pushPrompt pa (body >> return (Acc [])) >>= \(Acc l) -> return l

test_acc :: [Label]
test_acc = runIdentity . runCC . accumulated $
  enumerate (in_order tree1) acc