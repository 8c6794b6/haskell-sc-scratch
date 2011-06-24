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
-- * http://okmij.org/ftp/Haskell/Zipper1.hs
--
module Zipper.Zipper1 where

import Delimited.CCExc
import Control.Monad.Identity
import Control.Monad.Trans

data Term = Var String
          | A Term Term
          | L String Term
          deriving (Eq)

instance Show Term where
  show term = showt 0 term
    where showt _ (Var s)   = s
          showt p (A e1 e2) = paren (p > 10)
                                (showt 10 e1 ++ " " ++ showt 11 e2)
          showt p (L v e)   = paren (p > 0) ("L" ++ v ++ ". " ++ showt 0 e)
          paren True s  = "(" ++ s ++ ")"
          paren False s = s

data Direction = Down | DownRight | Up | Next
               deriving (Eq, Show)

traverse :: Monad m => (Term -> m (Maybe Term, Direction)) -> Term -> m Term
traverse tf term = do
  (term', direction) <- tf term
  let new_term = maybe term id term'
      select Up t = return t
      select Next t@(Var _) = return t
      select dir t@(L v t1) | dir == Next || dir == Down = do
        t1' <- traverse tf t1
        return $ L v t1'
      select DownRight t@(A t1 t2) = do
        t2' <- traverse tf t2
        return $ A t1 t2'
      select dir t@(A t1 t2) | dir == Next || dir == Down = do
        t1' <- traverse tf t1
        t2' <- traverse tf t2
        return $ A t1' t2'
  select direction new_term

term1 = L "f" (L "x" (A (A f (L "f" (A f (L "f" (L "x" x)))))
                        (A (A f (L "f" (L "x" x))) x)))
  where [f,x] = map Var ["f", "x"]

testt1 = runIdentity (traverse (\t -> return (Nothing,Next)) term1)

testt2 = traverse tf term1
  where tf term = print term >> return (Nothing,Next)

testt3 = traverse tf term1 where
  tf term@(A (Var "f") _) = do
    print "cutting" >> print term >> return (Nothing,Up)
  tf term = print term >> return (Nothing,Next)

testt4 = runIdentity (traverse tf term1) where
  tf (L "x" (Var "x")) = return (Just (L "y" (Var "y")),Next)
  tf _                 = return (Nothing,Next)

data Zipper m term dir
  = Zipper term ((Maybe term,dir) -> CCW m (Zipper m term dir))
  | ZipDone term

type CCM m term dir a = CC (PS (Zipper m term dir)) m a
type CCW m w = CC (PS w) m w

zip'term :: (Monad m) =>
  ((term -> CCM m term dir (Maybe term, dir)) -> term -> CCM m term dir term)
  -> term -> CCW m (Zipper m term dir)
zip'term trav term = pushPrompt ps (trav tf term >>= return . ZipDone)
  where tf term = shift0P ps (\k -> return (Zipper term k))

zip'through (ZipDone term) = liftIO (print "Done" >> print term)
zip'through (Zipper term k) = do
  liftIO $ print term
  k (Nothing,Next) >>= zip'through

tz1 :: IO ()
tz1 = runCC $ zip'term traverse term1 >>= zip'through

zip'move dir (Zipper term k) =
  liftIO (print dir >> print term) >> k (Nothing,dir)

zip'upr (Zipper term k) nt = do
  liftIO (print term >> print "replacing with" >> print nt)
  k (Just nt,Up)

zip'all'the'way'up :: Monad t =>
  Zipper t a Direction -> CC (PS (Zipper t a Direction)) t a
zip'all'the'way'up (ZipDone term)  = return term
zip'all'the'way'up (Zipper term k) = k (Nothing, Up) >>= zip'all'the'way'up

tz2 :: IO ()
tz2 = runCC $ do
  z <- zip'term traverse term1
  z1 <- zip'move Next z
  z1 <- zip'move Next z1
  z2 <- zip'move DownRight z1
  res <- zip'upr z2 (A (Var "x") (Var "x")) >>= zip'all'the'way'up
  liftIO $ print "Result" >> print res

data Direction1 = FirstKid | RightKid | Parent deriving (Eq, Show)

traverse1 :: (Monad m) =>
             (Term -> m (Maybe Term, Direction1)) -> Term -> m Term
traverse1 tf term = loop term where
  loop term = do
    (term', direction) <- tf term
    let new_term = maybe term id term'
        select Parent t = return t
        select dir t@(L v t1) | dir == FirstKid || dir == RightKid = do
          t1' <- loop t1
          loop $ L v t1'
        select RightKid t@(A t1 t2) = do
          t2' <- loop t2
          loop $ A t1 t2
        select FirstKid t@(A t1 t2) = do
          t1' <- loop t1
          loop $ A t1' t2
    select direction new_term

term2 = L "x" (A (Var "a") (Var "b"))

tz3 :: IO ()
tz3 = runCC $ do
  z <- zip'term traverse1 (L "x" (A (Var "a") (Var "b")))
  z <- zip'move FirstKid z
  z <- zip'move FirstKid z
  z <- zip'move Parent z
  z <- zip'move RightKid z
  z <- zip'move Parent z
  z <- zip'move Parent z
  ZipDone term <- zip'move Parent z
  liftIO $ print "Done:"
  liftIO $ print term