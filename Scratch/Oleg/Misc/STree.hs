{-|
Module      : $Header$
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* <http://okmij.org/ftp/Haskell/STrees.hs>

-}
module STree where

import Control.Monad
import System.Environment (getArgs)

-- --------------------------------------------------------------------------
-- * Introduction

{-$
Preventing memoization in (AI) search problems and non-deterministic
programming.

Or, how to build search trees and /avoid/ memoization implicit in lazy
evaluation.
-}

-- | An infinite strem of integers in 'MonadPlus'.
from :: MonadPlus m => Int -> m Int
from i = return i `mplus` from (i+1)

-- | Sample non-deterministic problem: computing an (infinite) stream
-- of Pythagorian triples.
--
pyth :: MonadPlus m => m (Int, Int, Int)
pyth = do
  x <- from 1
  y <- from 1
  z <- from 1
  if x*x + y*y == z*z then return (x,y,z) else mzero

-- --------------------------------------------------------------------------
-- * Take 1

{-$

> $ ghc -O2 -rtsopts -fforce-recomp -o one.out -main-is STree.main1 STree.hs
> $ ./one.out bfs 30 +RTS -s
> ...
>   46,900,946,408 bytes allocated in the heap
>    4,267,008,840 bytes copied during GC
>          754,112 bytes maximum residency (26 sample(s))
>           67,928 bytes maximum slop
>                3 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0     61358 colls,     0 par   12.81s   12.83s     0.0002s    0.0005s
>   Gen  1        26 colls,     0 par    0.03s    0.03s     0.0012s    0.0018s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time   27.90s  ( 27.95s elapsed)
>   GC      time   12.84s  ( 12.87s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time   40.74s  ( 40.81s elapsed)
>
>   %GC     time      31.5%  (31.5% elapsed)
>
>   Alloc rate    1,680,836,499 bytes per MUT second
>
>   Productivity  68.5% of total user, 68.4% of total elapsed
>
> $ ./one.out iter 30 +RTS -s
>
>       22,993,960 bytes allocated in the heap
>       21,039,920 bytes copied during GC
>        3,455,744 bytes maximum residency (6 sample(s))
>           49,904 bytes maximum slop
>                9 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0        38 colls,     0 par    0.03s    0.03s     0.0007s    0.0010s
>   Gen  1         6 colls,     0 par    0.03s    0.03s     0.0052s    0.0102s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time    0.09s  (  0.09s elapsed)
>   GC      time    0.06s  (  0.06s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time    0.15s  (  0.15s elapsed)
>
>   %GC     time      38.7%  (39.0% elapsed)
>
>   Alloc rate    249,200,306 bytes per MUT second
>
>   Productivity  61.2% of total user, 61.6% of total elapsed
>
> $ ./one.out iter 100 +RTS -s
> ...
>      347,834,072 bytes allocated in the heap
>      603,935,096 bytes copied during GC
>       46,774,768 bytes maximum residency (19 sample(s))
>          261,400 bytes maximum slop
>              123 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0       648 colls,     0 par    0.78s    0.78s     0.0012s    0.0023s
>   Gen  1        19 colls,     0 par    0.76s    0.76s     0.0402s    0.1314s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time    4.97s  (  4.98s elapsed)
>   GC      time    1.54s  (  1.54s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time    6.51s  (  6.52s elapsed)
>
>   %GC     time      23.7%  (23.7% elapsed)
>
>   Alloc rate    69,947,840 bytes per MUT second
>
>   Productivity  76.3% of total user, 76.2% of total elapsed

-}

-- | Representing the result of non-deterministic computation as an
-- infinite search tree.
--
-- First version of the search tree. One may say it is a free
-- MonadPlus.
data Tree1 a
  = Fail1
  | Val1 a
  | Node1 (Tree1 a) (Tree1 a)

instance Monad Tree1 where
  return = Val1
  Fail1       >>= f = Fail1
  Val1 x      >>= f = f x
  Node1 e1 e2 >>= f = Node1 (e1 >>= f) (e2 >>= f)

instance MonadPlus Tree1 where
  mzero = Fail1
  mplus = Node1

-- | Extracting the results from the search tree as a lazy list
-- (stream). The depth-first search is hopeless here. Doing
-- breadth-first search.
--
bfs1 :: Tree1 a -> [a]
bfs1 t = loop [t] where
  loop as = case as of
    []             -> []
    Fail1:xs       -> loop xs
    Val1 x:xs      -> x : loop xs
    Node1 e1 e2:xs -> loop (xs ++ [e1,e2])

-- | Testing 'bfs1'.
--
-- >>> testb1
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13)]
--
testb1 :: [(Int, Int, Int)]
testb1 = take 5 . bfs1 $ pyth

-- | Iterative deepening.
iter_deepening1 :: Tree1 a -> [a]
iter_deepening1 t = loop 0 where
  loop d = check d (depth_search1 d t)
  check d m = case m of
    Nothing -> []
    Just l  -> l ++ loop (d+1)

{-
XXX: See Searches.hs for more explanation.
-}

depth_search1 :: Int -> Tree1 a -> Maybe [a]
depth_search1 d t = case (d,t) of
  (0, Val1 x)      -> Just [x]
  (0, _     )      -> Just []
  (_, Fail1 )      -> Nothing
  (_, Val1 _)      -> Nothing
  (_, Node1 e1 e2) -> joinM (depth_search1 (d-1) e1) (depth_search1 (d-1) e2)

-- joinM is not:
--
-- > let f a b = (++) `fmap` a `ap` b
--
joinM :: Maybe [a] -> Maybe [a] -> Maybe [a]
joinM x y = case (x,y) of
  (Nothing, _)       -> y
  (_, Nothing)       -> x
  (Just l1, Just l2) -> Just (l1++l2)

-- | Testing 'iter_deepening1':
--
-- >>> testi1
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13)]
--
testi1 :: [(Int, Int, Int)]
testi1 = take 5 . iter_deepening1 $ pyth

-- | Main action builder.
main0 ::
  (Show a, MonadPlus m)
  => (m (Int, Int, Int) -> [a]) -> (m (Int, Int, Int) -> [a]) -> IO ()
main0 bfs iter_deepening = check =<< getArgs where
  check [key,ns] | [(n,"")] <- reads ns = print $ take n . select key $ pyth
  select k | k == "bfs"  = bfs
           | k == "iter" = iter_deepening

-- | Main function for take 1.
main1 :: IO ()
main1 = main0 bfs1 iter_deepening1

-- --------------------------------------------------------------------------
-- * Take 2

{-$
Attempting to prevent memoization by using explicit thunks.

> $ ghc-7.4.1 -O2 -main-is STree.main1 -o two.out -fforce-recomp -rtsopts STree.hs
> $ ./two.out bfs 30 +RTS -s
> ...
>   46,904,883,960 bytes allocated in the heap
>    4,271,185,136 bytes copied during GC
>          922,560 bytes maximum residency (32 sample(s))
>           69,936 bytes maximum slop
>                4 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0     61353 colls,     0 par   12.96s   12.99s     0.0002s    0.0004s
>   Gen  1        32 colls,     0 par    0.04s    0.05s     0.0014s    0.0022s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time   27.37s  ( 27.42s elapsed)
>   GC      time   13.01s  ( 13.04s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time   40.38s  ( 40.45s elapsed)
>
>   %GC     time      32.2%  (32.2% elapsed)
>
>   Alloc rate    1,713,371,331 bytes per MUT second
>
>   Productivity  67.8% of total user, 67.7% of total elapsed
>
> $ ./two.out iter 30 +RTS -s
> ...
>       21,386,568 bytes allocated in the heap
>       26,872,376 bytes copied during GC
>        6,090,288 bytes maximum residency (5 sample(s))
>           50,968 bytes maximum slop
>               15 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0        37 colls,     0 par    0.03s    0.03s     0.0009s    0.0012s
>   Gen  1         5 colls,     0 par    0.05s    0.05s     0.0093s    0.0211s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time    0.24s  (  0.24s elapsed)
>   GC      time    0.08s  (  0.08s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time    0.32s  (  0.32s elapsed)
>
>   %GC     time      24.9%  (25.0% elapsed)
>
>   Alloc rate    89,206,059 bytes per MUT second
>
>   Productivity  75.1% of total user, 75.2% of total elapsed
>
>
> $ ./two.out iter 100 +RTS -s
> ...
>      319,901,624 bytes allocated in the heap
>      575,308,064 bytes copied during GC
>       81,373,088 bytes maximum residency (12 sample(s))
>          312,872 bytes maximum slop
>              193 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0       603 colls,     0 par    0.77s    0.77s     0.0013s    0.0020s
>   Gen  1        12 colls,     0 par    0.91s    0.91s     0.0761s    0.2747s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time   11.26s  ( 11.28s elapsed)
>   GC      time    1.68s  (  1.68s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time   12.94s  ( 12.96s elapsed)
>
>   %GC     time      13.0%  (13.0% elapsed)
>
>   Alloc rate    28,395,467 bytes per MUT second
>
>   Productivity  87.0% of total user, 86.9% of total elapsed

-}

data Tree2 a
  = Fail2
  | Val2 a
  | Node2 (() -> Tree2 a) (() -> Tree2 a)

instance Monad Tree2 where
  return = Val2
  Fail2       >>= f = Fail2
  Val2 x      >>= f = f x
  Node2 e1 e2 >>= f = Node2 (\() -> e1 () >>= f) (\() -> e2 () >>= f)

instance MonadPlus Tree2 where
  mzero = Fail2
  mplus e1 e2 = Node2 (\() -> e1) (\() -> e2)

bfs2 :: Tree2 a -> [a]
bfs2 t = loop [\() -> t] where
  loop xs = case xs of
    []   -> []
    h:ys -> case h () of
      Fail2       -> loop ys
      Val2 x      -> x : loop ys
      Node2 e1 e2 -> loop (ys ++ [e1,e2])

-- | Testing 'bfs2'
--
-- >>> testb2
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13)]
--
testb2 :: [(Int,Int,Int)]
testb2 = take 5 . bfs2 $ pyth

depth_search2 :: Int -> Tree2 a -> Maybe [a]
depth_search2 d n = case (d,n) of
  (_, Fail2      ) -> Nothing
  (0, Val2 x     ) -> Just [x]
  (_, Val2 _     ) -> Nothing
  (0, _          ) -> Just []
  (_, Node2 e1 e2) ->
    joinM (depth_search2 (d-1) (e1 ())) (depth_search2 (d-1) (e2 ()))

iter_deepening2 :: Tree2 a -> [a]
iter_deepening2 t = loop 0 where
  loop d = check d (depth_search2 d t)
  check d m = case m of
    Nothing -> []
    Just l  -> l ++ loop (d+1)

-- | Testing 'iter_deepening2'
--
-- >>> iter_deepening2
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13)]
--
testi2 :: [(Int, Int, Int)]
testi2 = take 5 . iter_deepening2 $ pyth

main2 :: IO ()
main2 = main0 bfs2 iter_deepening2

-- --------------------------------------------------------------------------
-- * Take 3

{-$
The third version of the search tree.

Using thunks and additional trick to prevent the clever GHC memoize
even under lambda.

> $ ./three.out bfs 30 +RTS -s
> ...
>   46,904,691,024 bytes allocated in the heap
>    4,258,996,440 bytes copied during GC
>          717,032 bytes maximum residency (18 sample(s))
>           73,480 bytes maximum slop
>                3 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0     61378 colls,     0 par   13.36s   13.38s     0.0002s    0.0006s
>   Gen  1        18 colls,     0 par    0.03s    0.03s     0.0014s    0.0021s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time   27.88s  ( 27.92s elapsed)
>   GC      time   13.38s  ( 13.41s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time   41.26s  ( 41.33s elapsed)
>
>   %GC     time      32.4%  (32.4% elapsed)
>
>   Alloc rate    1,682,462,865 bytes per MUT second
>
>   Productivity  67.6% of total user, 67.4% of total elapsed
>
> $ ./three.out iter 30 +RTS -s
> ...
>      354,069,864 bytes allocated in the heap
>        6,388,096 bytes copied during GC
>          126,152 bytes maximum residency (3 sample(s))
>          211,568 bytes maximum slop
>                2 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0       679 colls,     0 par    0.02s    0.02s     0.0000s    0.0001s
>   Gen  1         3 colls,     0 par    0.00s    0.00s     0.0003s    0.0004s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time    0.59s  (  0.59s elapsed)
>   GC      time    0.02s  (  0.02s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time    0.62s  (  0.62s elapsed)
>
>   %GC     time       3.8%  (3.8% elapsed)
>
>   Alloc rate    597,017,150 bytes per MUT second
>
>   Productivity  96.1% of total user, 96.2% of total elapsed
>
> $ ./three.out iter 100 +RTS -s
> ...
>   12,977,276,424 bytes allocated in the heap
>      139,031,256 bytes copied during GC
>          613,568 bytes maximum residency (78 sample(s))
>          211,568 bytes maximum slop
>                3 MB total memory in use (0 MB lost due to fragmentation)
>
>                                     Tot time (elapsed)  Avg pause  Max pause
>   Gen  0     24845 colls,     0 par    0.56s    0.56s     0.0000s    0.0006s
>   Gen  1        78 colls,     0 par    0.05s    0.05s     0.0007s    0.0013s
>
>   INIT    time    0.00s  (  0.00s elapsed)
>   MUT     time   21.10s  ( 21.14s elapsed)
>   GC      time    0.62s  (  0.62s elapsed)
>   EXIT    time    0.00s  (  0.00s elapsed)
>   Total   time   21.72s  ( 21.76s elapsed)
>
>   %GC     time       2.8%  (2.8% elapsed)
>
>   Alloc rate    614,877,927 bytes per MUT second
>
>   Productivity  97.2% of total user, 97.0% of total elapsed

-}

data Tree3 a
  = Fail3
  | Val3 a
  | Node3 (() -> Tree3 a) (() -> Tree3 a)

instance Monad Tree3 where
  return = Val3
  Fail3       >>= f = Fail3
  Val3 x      >>= f = f x
  Node3 e1 e2 >>= f = Node3 (app1 e1 f) (app1 e2 f)

instance MonadPlus Tree3 where
  mzero = Fail3
  mplus e1 e2 = Node3 (app e1) (app e2)

app e () = e
{-# NOINLINE app #-}

app1 e f () = e () >>= f
{-# NOINLINE app1 #-}

bfs3 :: Tree3 a -> [a]
bfs3 t = loop [\() -> t] where
  loop xs = case xs of
    []   -> []
    h:ys -> case h () of
      Fail3       -> loop ys
      Val3 x      -> x : loop ys
      Node3 e1 e2 -> loop (ys ++ [e1,e2])

-- | Testing 'bfs3'
--
-- >>> testb3
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13)]
--
testb3 :: [(Int, Int, Int)]
testb3 = take 5 . bfs3 $ pyth

depth_search3 :: Int -> Tree3 a -> Maybe [a]
depth_search3 d n = case (d,n) of
  (_, Fail3      ) -> Nothing
  (0, Val3 x     ) -> Just [x]
  (_, Val3 _     ) -> Nothing
  (0, _          ) -> Just []
  (_, Node3 e1 e2) ->
    joinM (depth_search3 (d-1) (e1 ())) (depth_search3 (d-1) (e2 ()))

iter_deepening3 :: Tree3 a -> [a]
iter_deepening3 t = loop 0 where
  loop d = check d (depth_search3 d t)
  check d m = case m of
    Nothing -> []
    Just l  -> l ++ loop (d+1)

-- | Testing 'iter_deepening3':
--
-- >>> testi3
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13)]
--
testi3 :: [(Int,Int,Int)]
testi3 = take 5 . iter_deepening3 $ pyth

main3 :: IO ()
main3 = main0 bfs3 iter_deepening3