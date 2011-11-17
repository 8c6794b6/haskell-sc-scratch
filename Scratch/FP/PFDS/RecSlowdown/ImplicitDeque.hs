{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading /purely functional data structure/,
by Chris Okasaki.

Implicit deque, from figure 8.2.

-}
module RecSlowdown.ImplicitDeque where

import Prelude hiding (head, tail, last, init)
import qualified Prelude as P
import Queue.Initial (emptyQueueException)

import Criterion.Main
import System.Random

data D a
  = Zero
  | One a
  | Two a a
  | Three a a a
  deriving (Show)

data Queue a
  = Shallow (D a)
  | Deep (D a) (Queue (a,a)) (D a)

instance Show a => Show (Queue a) where
  show q = "Queue " ++ showList (toList q) ""

empty :: Queue a
empty = Shallow Zero

isEmpty :: Queue a -> Bool
isEmpty q = case q of Shallow Zero -> True; _ -> False

toList :: Queue a -> [a]
toList q = case q of
  Shallow d  -> dlist d
  Deep f m r -> dlist f ++ concatMap (\(a,b) -> [a,b]) (toList m) ++ dlist r

dcons :: a -> D a -> D a
dcons x d = case d of
  Zero    -> One x
  One a   -> Two x a
  Two a b -> Three x a b

dsnoc :: a -> D a -> D a
dsnoc x d = case d of
  Zero    -> One x
  One a   -> Two a x
  Two a b -> Three a b x

dhead :: D a -> a
dhead d = case d of
  Zero        -> emptyQueueException
  One a       -> a
  Two a _     -> a
  Three a _ _ -> a

dlast :: D a -> a
dlast d = case d of
  Zero        -> emptyQueueException
  One a       -> a
  Two _ a     -> a
  Three _ _ a -> a

dtail :: D a -> D a
dtail d = case d of
  Zero        -> emptyQueueException
  One a       -> Zero
  Two _ b     -> One b
  Three _ a b -> Two a b

dinit :: D a -> D a
dinit d = case d of
  Zero        -> emptyQueueException
  One a       -> Zero
  Two a _     -> One a
  Three a b _ -> Two a b

dlist :: D a -> [a]
dlist d = case d of
  Zero        -> []
  One a       -> [a]
  Two a b     -> [a,b]
  Three a b c -> [a,b,c]

cons :: a -> Queue a -> Queue a
cons x q = case q of
  Shallow (Three a b c)   -> Deep (Two x a) empty (Two b c)
  Shallow d               -> Shallow (dcons x d)
  Deep (Three a b c) ~m r -> Deep (Two x a) (cons (b, c) m) r
  Deep f ~m r              -> Deep (dcons x f) m r

head :: Queue a -> a
head q = case q of
  Shallow d  -> dhead d
  Deep f _ _ -> dhead f

tail :: Queue a -> Queue a
tail q = case q of
  Shallow d -> Shallow (dtail d)
  Deep (One a) ~m r
    | isEmpty m -> Shallow r
    | otherwise -> case head m of (b,c) -> Deep (Two b c) (tail m) r
  Deep f ~m r -> Deep (dtail f) m r

snoc :: a -> Queue a -> Queue a
snoc x q = case q of
  Shallow (Three a b c)  -> Deep (Two a b) empty (Two c x)
  Shallow d              -> Shallow (dsnoc x d)
  Deep f ~m (Three a b c) -> Deep f (snoc (a, b) m) (Two c x)
  Deep f ~m r             -> Deep f m (dsnoc x r)

last :: Queue a -> a
last q = case q of
  Shallow d -> dlast d
  Deep _ _ r -> dlast r

init :: Queue a -> Queue a
init q = case q of
  Shallow d -> Shallow (dinit d)
  Deep f ~m (One a)
    | isEmpty m -> Shallow f
    | otherwise -> case last m of (b,c) -> Deep f (init m) (Two b c)
  Deep f ~m r -> Deep f m (dinit r)

{-
Catenable double-ended Queues (from chapter 8.5)
Amortimized time of head, tail, cons, snoc, last, init are O(1).
-}

mkQueue :: Int -> StdGen -> Queue Int
mkQueue n g
  | n == 0 = empty
  | even n = case random g of (!x,!g') -> cons x (mkQueue (n-1) g')
  | odd  n = case random g of (!x,!g') -> snoc x (mkQueue (n-1) g')

mkList :: Int -> StdGen -> [Int]
mkList n g
  | n == 0    = []
  | otherwise = case random g of (!x,!g') -> x : mkList (n-1) g'

main :: IO ()
main = do
  g <- newStdGen

  let mk f c n k = bench (n++" n="++show k) (whnf f (c k g))
      qops k =
        [ mk head mkQueue "head" k
        , mk tail mkQueue "tail" k
        , mk init mkQueue "init" k
        , mk last mkQueue "last" k ]
      lops k =
        [ mk P.head mkList "head" k
        , mk P.tail mkList "tail" k
        , mk P.init mkList "init" k
        , mk P.last mkList "last" k ]

  defaultMain
    [ bgroup "n=100"
       [ bgroup "queue" (qops 100)
       , bgroup "list" (lops 100) ]
    , bgroup "n=1000"
       [ bgroup "queue" (qops 1000)
       , bgroup "list" (lops 1000) ]
    , bgroup "n=10000"
       [ bgroup "queue" (qops 10000)
       , bgroup "list" (lops 10000) ]
    ]
