{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/Haskell/Fix.hs>
-}
module Fix where

import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Data.Dynamic

fact self 0 = 1
fact self n = n * self (pred n)

newtype Wrap a = Wrap {unwrap :: Wrap a -> a}
fix1 f = let aux g = g (Wrap g) 
         in  aux (\x -> f (unwrap x x))
             
fix2 f =              
  let wrap = toDyn
      unwrap x = fromDyn x undefined
      aux g = g (wrap g) 
  in  aux (\x -> f (unwrap x x))
      
data W3 a = forall d. C d => W3 (d a -> a)      

class C d where
  unwrap3 :: (d a -> a) -> W3 a -> a
  
instance C W3 where
  unwrap3 = ($)
  
fix3 f = let aux g = g (W3 g)   
         in  aux (\x -> f (case x of W3 h -> unwrap3 h x))
             
type family D a :: *             
     
newtype W31 a = W31 (D a -> a)

type instance D a = W31 a

fix31 f = let aux g = g (W31 g)
          in  aux (\x -> f (case x of W31 h -> h x))
              
fix4 f = runST $ do              
  wrap <- newSTRef (error "black hole")
  let aux = readSTRef wrap >>= (\x -> x >>= return . f)
  writeSTRef wrap aux
  aux
  
test1 = fix1 fact 5  
test2 = fix2 fact (5::Int)
test3 = fix3 fact 5
test31 = fix31 fact 5
test4 = fix4 fact 5
