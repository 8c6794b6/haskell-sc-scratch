------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with perfect random shuffle.
--
module PerfectRandom where

import System.Random

extract :: Integer -> [a] -> (a, [a])
extract 0 (h:t) = (h,t)
extract n l = loop n l []
  where
    loop 0 (h:t) accum = (h, accum ++ t)
    loop n (h:t) accum = loop (n-1) t (h:accum)

shuffle :: [b] -> [Integer] -> [b]
shuffle [e] [] = [e]
shuffle elements (r:r_others) = b:(shuffle rest r_others)
  where (b,rest) = extract r elements

data Tree a = Leaf a
            | Node !Int (Tree a) (Tree a)
            deriving (Show)

build_tree = grow_level . (map Leaf)
  where
    grow_level [node] = node
    grow_level l = grow_level $ inner l

    inner [] = []
    inner x@[_] = x
    inner (e1:e2:rest) = (join e1 e2) : inner rest

    join l r = case (l,r) of
      (Leaf _,Leaf _)             -> Node 2 l r
      (Node ct _ _,Leaf _)        -> Node (ct+1) l r
      (Leaf _,Node ct _ _)        -> Node (ct+1) l r
      (Node ctl _ _,Node ctr _ _) -> Node (ctl+ctr) l r


shuffle1 :: [a] -> [Int] -> [a]
shuffle1 elems rseq = shuffle1' (build_tree elems) rseq
  where
    shuffle1' (Leaf e) [] = [e]
    shuffle1' tree (ri:r_others) =
      extract_tree ri tree (\t -> shuffle1' t r_others)

    extract_tree 0 (Node _ (Leaf e) r) k = e:k r
    extract_tree 1 (Node 2 l@Leaf{} (Leaf e)) k = e:k l
    extract_tree n (Node c l@Leaf{} r) k =
      extract_tree (n-1) r (\r' -> k $ Node (c-1) l r')
    extract_tree n (Node n1 l (Leaf e)) k | n+1 == n1 = e:k l
    extract_tree n (Node c l@(Node cl _ _) r) k
      | n < cl    = extract_tree n l (\l' -> k $ Node (c-1) l' r)
      | otherwise = extract_tree (n-cl) r (\r' -> k $ Node (c-1) l r')

t1 = shuffle1 "abcde" [0,0,0,0]
t2 = shuffle1 "abcde" [4,3,2,1]
t3 = shuffle1 "abcde" [2,1,2,0]

make_rs :: RandomGen g => Int -> g -> ([Int],g)
make_rs n g = loop [] n g
  where
    loop acc 0 g = (reverse acc, g)
    loop acc n g = loop (r:acc) (pred n) g'
      where (r,g') = randomR (0,n) g

shuffle2 :: RandomGen g => [a] -> g -> [a]
shuffle2 xs g = shuffle1 xs (fst $ make_rs (length xs - 1) g)

main = let n = 1000000
       in  print $ length $ shuffle1 [1..n+1] (fst $ make_rs n (mkStdGen 17))
