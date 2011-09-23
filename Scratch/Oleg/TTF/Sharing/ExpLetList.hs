{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

DSL Sharing tutorial from:

* <http://okmij.org/ftp/tagless-final/sharing/ExpLetList.hs>

-}
module ExpLetList where

import ExpF (N(..), NodeId, DAG(..), Exp(..), do_bench)
import ExpLet (ExpLet(..))
import BiMap (empty)
import Control.Monad.State 

class LC n m a where
  gseq :: [n a] -> m [a]
  gcom :: m [a] -> ([n a] -> m [a]) -> m [a]
  
instance Monad m => LC m m a where
  gseq = sequence
  gcom m f = m >>= f . map return
  
instance LC N (State DAG) Int where  
  gseq xs = sequence (map unN xs)
  gcom m f = m >>= f . map (N . return)
  
{-

Original sklansky

sklansky :: (a -> a -> a) -> [a] -> [a]
sklansky f [] = []
sklansky f [x] = [x]
sklansky f xs = left' ++ [f (last left') r | r <- right' ]
  where
    (left, right) = splitAt (length xs `div` 2) xs
    left' = sklansky f left
    right' = sklansky f right

-}

sklansky' :: LC m n a 
   => (m a -> m a -> m a) -> [m a] -> n [a]
sklansky' f a@[] = gseq a   
sklansky' f [x] = gseq [x]
sklansky' f xs = 
  gcom left' $ \left'' ->
  gcom right' $ \right'' ->
  gseq $ left'' ++ [ f (last left'') r | r <- right'' ]
  where
    (left,right) = splitAt (length xs `div` 2) xs
    left'  = sklansky' f left
    right' = sklansky' f right
    
test_sklansky' n = runState sk (DAG empty) where
  sk = sklansky' add xs
  xs = (map (variable . show) [1..n]) :: [N Int]
  
bench_skl' n = do_bench $ test_sklansky' n

main = do
  print $ bench_skl' 4096