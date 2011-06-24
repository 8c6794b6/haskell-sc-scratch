{-# LANGUAGE PatternGuards, KindSignatures #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes, ImpredicativeTypes #-}

module Delimited.CCCxe where

import Control.Monad.Trans
import Data.Typeable

newtype CC p m a =
  CC { unCC :: forall w. (a -> m w) ->
               (forall x. SubCont p m x a -> p m x -> m w) -> m w }

type SubCont p m a b = CC p m a -> CC p m b

type CCT p m a w = SubCont p m a w -> CC p m w

type Prompt p m w =
  (forall x. CCT p m x w -> p m x,
   forall x. p m x -> Maybe (CCT p m x w))

instance Monad m => Monad (CC p m) where
  return x = CC $ \ki kd -> ki x
  m >>= f = CC $ \ki kd -> unCC m
    (\a -> unCC (f a) ki kd)
    (\ctx -> kd (\x -> ctx x >>= f))

instance MonadTrans (CC p) where
  lift m = CC $ \ki kd -> m >>= ki

instance MonadIO m => MonadIO (CC p m) where
  liftIO = lift . liftIO

pushPrompt :: Monad m => Prompt p m w -> CC p m w -> CC p m w
pushPrompt p@(_,proj) body = CC $ \ki kd ->
  let kd' ctx body | Just b <- proj body = unCC (b ctx) ki kd
      kd' ctx body = kd (\x -> pushPrompt p (ctx x)) body
  in  unCC body ki kd'

takeSubCont :: Monad m => Prompt p m w -> CCT p m x w -> CC p m x
takeSubCont p@(inj,_) body = CC $ \ki kd -> kd id (inj body)

pushSubCont :: Monad m => SubCont p m a b -> CC p m a -> CC p m b
pushSubCont = ($)

runCC :: Monad m => CC (p :: (* -> *) -> * -> *) m a -> m a
runCC m = unCC m return err where
  err = error "Escaping bubble: you have forgotten pushPrompt"

abortP :: Monad m => Prompt p m w -> CC p m w -> CC p m any
abortP p e = takeSubCont p (\_ -> e)

shiftP :: Monad m =>
          Prompt p m w -> ((a -> CC p m w) -> CC p m w) -> CC p m a
shiftP p f =
  takeSubCont p $ \sk ->
    pushPrompt p (f (\c ->
      pushPrompt p (pushSubCont sk (return c))))

shift0P :: Monad m =>
           Prompt p m w -> ((a -> CC p m w) -> CC p m w) -> CC p m a
shift0P p f =
  takeSubCont p $ \sk ->
  f $ \c ->
  pushPrompt p $ pushSubCont sk $ return c

controlP :: Monad m =>
            Prompt p m w -> ((a -> CC p m w) -> CC p m w) -> CC p m a
controlP p f =
  takeSubCont p $ \sk ->
  pushPrompt p $ f $ \c ->
  pushSubCont sk $ return c

newtype PS w m x = PS (CCT (PS w) m x w)

ps :: Prompt (PS w) m w
ps = (inj, prj) where
  inj = PS
  prj (PS x) = Just x

newtype P2 w1 w2 m x =
  P2 (Either (CCT (P2 w1 w2) m x w1) (CCT (P2 w1 w2) m x w2))

p2L :: Prompt (P2 w1 w2) m w1
p2L = (inj, prj) where
  inj = P2 . Left
  prj (P2 (Left x)) = Just x
  prj _             = Nothing


p2R :: Prompt (P2 w1 w2) m w2
p2R = (inj, prj) where
  inj = P2 . Right
  prj (P2 (Right x)) = Just x
  prj _              = Nothing

data PP m x = forall w. Typeable w => PP (CCT PP m x w)

newtype NCCT p m a w = NCCT {unNCCT :: CCT p m a w}

pp :: Typeable w => Prompt PP m w
pp = (inj, prj) where
  inj = PP
  prj (PP c) = maybe Nothing (Just . unNCCT) (gcast (NCCT c))

data PM c m x = forall w. Typeable w => PM (CCT (PM c) m x w)

pm :: Typeable w => Prompt (PM c) m w
pm = (inj, prj) where
  inj = PM
  prj (PM c) = maybe Nothing (Just . unNCCT) (gcast (NCCT c))

data PD m x = forall w. Typeable w => PD Int (CCT PD m x w)

newPrompt :: Typeable w => Int -> Prompt PD m w
newPrompt mark = (inj,prj) where
  inj = PD mark
  prj (PD mark' c) | mark' == mark,
                     Just (NCCT x) <- gcast (NCCT c) = Just x
  prj _            = Nothing

as_prompt_type :: Prompt p m w -> w -> Prompt p m w
as_prompt_type = const
