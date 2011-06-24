{-# LANGUAGE PatternGuards #-}
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
-- * http://okmij.org/ftp/Haskell/ZipperTraversable.hs
--
module Zipper.ZipperTraversable where

import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.Tree as Tree

data Zipper t a = ZDone (t a)
                | Z a (Maybe a -> Zipper t a)

make_zipper :: T.Traversable t => t a -> Zipper t a
make_zipper t = reset $ T.mapM f t >>= return . ZDone where
  f a = shift (\k -> return $ Z a (k . maybe a id))

zip_up :: Zipper t a -> t a
zip_up (ZDone t) = t
zip_up (Z _ k)   = zip_up $ k Nothing

newtype Cont r a = Cont { runCont :: (a->r) -> r }

instance Monad (Cont r) where
  return x = Cont $ \k -> k x
  m >>= f  = Cont $ \k -> runCont m (\v -> runCont (f v) k)

reset :: Cont r r -> r
reset m = runCont m id

shift :: ((a->r) -> Cont r r) -> Cont r a
shift e = Cont $ \k -> reset (e k)


-- Tests

tmap = M.fromList [(v,product [1..v]) | v <- [1..10]]

trav t = [a1,a3,a4] where
  (Z a1 k1) = make_zipper t
  (Z a2 k2) = k1 Nothing
  (Z a3 k3) = k2 Nothing
  (Z a4 k4) = k3 Nothing

travm = trav tmap

tmod t = loop (make_zipper t) where
  loop (ZDone t) = putStrLn $ "Done\n: " ++ show t
  loop (Z a k)   = do
    putStrLn $ "Current element: " ++ show a
    ask k
  ask k = do
    putStrLn "Enter Return, q or the replacement value: "
    getLine >>= check k
  check k ""                     = loop $ k Nothing
  check k "\r"                   = loop $ k Nothing
  check k ('q':_)                = loop . ZDone . zip_up $ k Nothing
  check k s | [(n,_)] <- reads s = loop $ k (Just n)
  check k _                      = putStrLn "Repeat" >> ask k

testm :: IO ()
testm = tmod tmap
