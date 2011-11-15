{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

Scheduled bottom-Up mergesort in chapter 4.

-}
module ElimAmort.Scheduled where

newtype Schedule a = Schedule [[a]]

type Less a = a -> a -> Bool

data Sortable a = Sortable 
  { less :: Less a
  , size :: Int
  , segments :: [([a],Schedule a)] }

merge :: Less a -> [a] -> [a] -> [a]
merge f xs ys = case (xs,ys) of
  ([],_) -> ys
  (_,[]) -> xs
  (x:xs', y:ys')
    | f x y     -> x : merge f xs' ys
    | otherwise -> y : merge f xs ys'

exec1 :: Schedule a -> Schedule a
exec1 (Schedule xs) = Schedule (exec1' xs) where
  exec1' ys = case ys of
    []            -> []
    []:sched      -> exec1' sched
    (y:ys'):sched -> y `seq` ys' : sched

exec2PerSeg :: [([a], Schedule a)] -> [([a], Schedule a)]
exec2PerSeg xs = case xs of
  [] -> []
  (ys,sched):segs -> (ys, exec1 (exec1 sched)) : exec2PerSeg segs

new :: Less a -> Sortable a
new f = Sortable f 0 []

add :: a -> Sortable a -> Sortable a
add x (Sortable f sz segs) =
  let addSeg xs sgs z (Schedule rsched)
        | z `mod` 2 == 0 = (xs, Schedule (reverse (xs:rsched))) : sgs
        | otherwise      = 
          let (xs', _) : sgs' = sgs
          in  addSeg (merge f xs xs') sgs' (z `div` 2) (Schedule (xs:rsched))
      segs' = addSeg [x] segs sz (Schedule [])
  in  Sortable f (succ sz) (exec2PerSeg segs')
        
sort :: Sortable a -> [a]
sort (Sortable f _ segs) =
  let mergeAll xs [] = xs
      mergeAll xs ((xs',_):sgs) = mergeAll (merge f xs xs') sgs
  in  mergeAll [] segs