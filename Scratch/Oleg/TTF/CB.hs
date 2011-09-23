{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/tagless-final/CB.hs>

-}
module CB where

import Control.Monad
import Control.Monad.Trans
import Data.IORef

data IntT
data a :-> b
infixr 5 :->

class EDSL exp where
  lam :: (exp a -> exp b) -> exp (a :-> b)
  app :: exp (a :-> b) -> exp a -> exp b
  int :: Int -> exp IntT
  add :: exp IntT -> exp IntT -> exp IntT
  sub :: exp IntT -> exp IntT -> exp IntT
  
let_ :: EDSL exp => exp a -> (exp a -> exp b) -> exp b  
let_ x y = (lam y) `app` x

t :: EDSL exp => exp IntT
t = (lam $ \x -> let_ (x `add` x)
                 $ \y -> y `add` y) `app` int 10
    
type family Sem (m :: * -> *) a :: *    
type instance Sem m IntT = Int
type instance Sem m (a :-> b) = m (Sem m a) -> m (Sem m b)

newtype S l m a = S {unS :: m (Sem m a)}

------------------------------------------------------------------------------
-- Call-by-name

data Name

instance MonadIO m => EDSL (S Name m) where
  int = S . return 
  add x y = S $ do
    a <- unS x
    b <- unS y
    liftIO $ putStrLn "Adding"
    return $ a + b
  sub x y = S $ do
    a <- unS x
    b <- unS y
    liftIO $ putStrLn "Subtracting"
    return $ a - b
  lam f = S . return $ (unS . f . S)
  app x y = S $ unS x >>= ($ (unS y))
  
runName :: S Name m a -> m (Sem m a)  
runName x = unS x

t0SN :: IO ()
t0SN = print =<< runName t 
{- 
ghci> t0SN
Adding
Adding
Adding
40
-}

t1 :: EDSL exp => exp IntT
t1 = (lam $ \x -> let_ (x `add` x) 
                  $ \y -> lam $ \z -> 
       z `add` (z `add` (y `add` y)))
     `app` (int 10 `sub` int 5)
     `app` (int 20 `sub` int 10)
{-     
ghci> runName t1
Subtracting
Subtracting
Subtracting
Subtracting
Adding
Subtracting
Subtracting
Adding
Adding
Adding
Adding
40
-}

-- Result of subtraction is not needed.
t2 :: EDSL exp => exp IntT
t2 = (lam $ \z -> lam $ \x -> let_ (x `add` x) $ \y -> y `add` y)
     `app` (int 100 `sub` int 10)
     `app` (int 5 `add` int 5)
     
{-  
ghci> runName t2
Adding
Adding
Adding
Adding
Adding
Adding
Adding
80
-}

------------------------------------------------------------------------------
-- Call-by-value

data Value

vn :: S Value m x -> S Name m x
vn = S . unS

nv :: S Name m x -> S Value m x
nv = S . unS

-- | The only difference between CBN and CBV is lam.
-- lam first evaluates its argument, no matter what.
-- This is the definition of CBV after all.
instance MonadIO m => EDSL (S Value m) where
  int = nv . int
  add x y = nv $ add (vn x) (vn y)
  sub x y = nv $ sub (vn x) (vn y)
  app x y = nv $ app (vn x) (vn y)
  lam f = S . return $ (\x -> x >>= unS . f . S . return)
  
runValue :: S Value m a -> m (Sem m a)
runValue x = unS x

t0SV = runValue t >>= print

{-
ghci> runValue t
Adding
Adding
40

ghci> runValue t1
Subtracting
Adding
Subtracting
Adding
Adding
Adding
40

In below, unused sub expression is evaluated.

ghci> runValue t2
Subtracting
Adding
Adding
Adding
80

-}

------------------------------------------------------------------------------
-- Call-by-need

share :: MonadIO m => m a -> m (m a)
share m = do
  r <- liftIO $ newIORef (False,m)
  let ac = do
        (f,m) <- liftIO $ readIORef r
        if f then m 
          else do
            v <- m
            liftIO $ writeIORef r (True,return v)
            return v
  return ac
  
data Lazy  

ln :: S Lazy m x -> S Name m x
ln = S . unS
nl :: S Name m x -> S Lazy m x
nl = S . unS

-- | The only difference between CBN and CBNeed is the use of share in
-- lam. lam shares its argument, no matter what. This is the definition of
-- call-by-need after all.
instance MonadIO m => EDSL (S Lazy m) where
  int = nl . int
  add x y = nl $ add (ln x) (ln y)
  sub x y = nl $ sub (ln x) (ln y)
  app x y = nl $ app (ln x) (ln y)
  lam f = S . return $ (\x -> share x >>= unS . f . S)   
          
runLazy :: S Lazy m a -> m (Sem m a)
runLazy x = unS x

{-

ghci> runLazy t
Adding
Adding
40

ghci> runLazy t1
Adding
Adding
Adding
Adding
Adding
Adding
120

Not showing "Subtracting". 
Subtraction was bounded but not used in t2 expression.

ghci> runLazy t2
Adding
Adding
Adding
80

-}

main = do
  putStrLn "---- call-by-name ----"
  print =<< runName t 
  print =<< runName t1
  print =<< runName t2
  putStrLn "---- call-by-value ----"
  print =<< runValue t
  print =<< runValue t1 
  print =<< runValue t2
  putStrLn "---- call-by-need ----"
  print =<< runLazy t
  print =<< runLazy t1
  print =<< runLazy t2
