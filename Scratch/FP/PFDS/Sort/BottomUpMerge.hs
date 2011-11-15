{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

Merge sort, from chapter 3.5.

ML code for this chapter: 

> signature Sortable = 
> sig 
>   type a Sortable
>   val new  : {Less a * a -> bool} -> a Sortable (* sort in increasing order by Less *)
>   val add  : a * a Sortable -> a Sortable
>   val sort : a Sortable -> a list
> end

-}
module Sort.BottomUpMerge where

-- XXX: Criterion package occasionaly brake ...
-- 
-- import Criterion.Main
-- 

import System.Random
import Control.DeepSeq
import qualified Data.List as L

type Less a = a -> a -> Bool

data Sortable a = Sortable
  { less :: Less a
  , size :: Int
  , segments :: [[a]]
  } 
  
instance NFData a => NFData (Sortable a) where
  rnf (Sortable _ sz segs) = rnf sz `seq` rnf segs
  
instance Show a => Show (Sortable a) where  
  show (Sortable _ sz segs) = "Sortable (size " ++ show sz ++ ") " ++ show segs

merge :: Less a -> [a] -> [a] -> [a]
merge less xs ys = mrg xs ys where
  mrg as [] = as
  mrg [] bs = bs
  mrg as'@(a:as) bs'@(b:bs)
    | less a b  = a : mrg as bs'
    | otherwise = b : mrg as' bs

add :: a -> Sortable a -> Sortable a
add x (Sortable f sz segs) =
  let addSeg ys yss sz'
        | sz' `mod` 2 == 0 = ys:yss
        | otherwise        = addSeg (merge f ys (head yss)) (tail yss) (sz' `div` 2)
  in  Sortable f (sz+1) (addSeg [x] segs sz)

sort :: Sortable a -> [a]
sort (Sortable f _ segs) = 
  let mergeAll xs [] = xs
      mergeAll xs (ys:yss) = mergeAll (merge f xs ys) yss
  in  mergeAll [] $! segs
      
sort' :: Sortable a -> [a]      
sort' (Sortable f _ segs)= L.foldl' (merge f) [] segs
      
new :: Less a -> Sortable a
new f = Sortable f 0 []

-- main :: IO ()
-- main = do
--   seed <- newStdGen
--   let n = 100
--   defaultMain 
--     [ bench "list" (nf L.sort (mkList n seed))
--     , bench "sortable" (nf sort (mkSortable n seed))
--     ]
  
mkList :: Int -> StdGen -> [Int]
mkList n seed = case n of
  0 -> []
  _ -> let (x,seed') = random seed 
       in  x : mkList (n-1) seed'
        
mkSortable :: Int -> StdGen -> Sortable Int        
mkSortable n seed = case n of
  0 -> new (<)
  _ -> let (x,seed') = random seed
       in  add x (mkSortable (n-1) seed')