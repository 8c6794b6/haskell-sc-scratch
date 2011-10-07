{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

TemplateHaskell helpers.

Node matching expressions in Pdouble are:

>  Node "*@" [e1,e2] -> do
>    Term TyDouble v1 <- self (e1,g)
>    Term TyDouble v2 <- self (e2,g)
>    return $ Term TyDouble $ v1 *@ v2

or:

>  Node "pmidiCPS" [e1] -> do
>    Term TyDouble v1 <- self (e1,g)
>    return $ Term TyDouble $ pmidiCPS v1

When applying each class member function, type variable for
variable env may vary, cannot put all functions in list, and lookup it.

... or is there a way to do same thing without using TH?

-}
module Scratch.Pattern.THHelper where

import Language.Haskell.TH

import Scratch.Pattern.PC02
import Scratch.Pattern.Term00
import Scratch.Pattern.Type00
import Scratch.Pattern.Etree

-- | Show contents after applying ppr.
printQ :: Ppr a => Q a -> IO ()
printQ x = putStrLn =<< (runQ $ (show . ppr) `fmap` x)

-- | Show raw TH data.
showQ :: Show a => Q a -> IO ()
showQ x = putStrLn =<< (runQ $ show `fmap` x)

-- | Matching pattern for Pdouble with single argument.
dmatch1s :: Name -> [String] -> Name -> Name -> Name -> Name -> ExpQ
dmatch1s name names self e1 g other =
  let mkPat n = match (litP (StringL n))
                (normalB $ dmatch1 self e1 g (mkName n)) []
      other' = match wildP (normalB (varE other)) []
  in  caseE (varE name) ((map mkPat names) ++ [other'])

-- | Matching pattern for Pdouble with 2 arguments.
dmatch2s :: Name -> [String] -> Name -> Name -> Name -> Name -> Name -> ExpQ
dmatch2s name names self e1 e2 g other =
  let mkPat n = match (litP (StringL n))
                (normalB $ dmatch2 self e1 e2 g (mkName n)) []
      other' = match wildP (normalB (varE other)) []
  in  caseE (varE name) ((map mkPat names) ++ [other'])

-- | Match with single Double argument.
dmatch1 :: Name -> Name -> Name -> Name -> ExpQ
dmatch1 = match1 'TyDouble

-- | Match with 2 Double argument
dmatch2 :: Name -> Name -> Name -> Name -> Name -> ExpQ
dmatch2 = match2 'TyDouble

-- | Match with single Int argument.
imatch1 :: Name -> Name -> Name -> Name -> ExpQ
imatch1 = match1 'TyInt

-- | Match with 2 Int arguments.
imatch2 :: Name -> Name -> Name -> Name -> Name -> ExpQ
imatch2 = match2 'TyInt

-- | Body of deserialization for single argument function.
match1 :: Name -> Name -> Name -> Name -> Name -> ExpQ
match1 typ self e g fun =
  let v = varE; self' = v self; e' = v e; g' = v g;
      fun' = v fun; typ' = conE typ
  in  [e| do Term ty v1 <- $self' ($e',$g')
             case cmpTy ty $typ' of
               Just Equal -> return $ Term $typ' ($fun' v1)
        |]

-- | Body of deserialization for 2 arguments function.
match2 :: Name -> Name -> Name -> Name -> Name -> Name -> ExpQ
match2 typ self e1 e2 g fun =
  let v = varE; self' = v self; e1' = v e1; e2' = v e2;
      g' = v g; fun' = v fun; typ' = conE typ
  in  [e| do Term t1 v1 <- $self' ($e1',$g')
             Term t2 v2 <- $self' ($e2',$g')
             case (cmpTy t1 $typ',cmpTy t2 $typ') of
               (Just Equal,Just Equal) -> return $ Term $typ' ($fun' v1 v2)
        |]

-- | Recursively call deserialization function with comparing result type
-- and appending each value to build a list from deserialized value.
listRec :: Name -> Name -> Name -> ExpQ
listRec self t1 g =
   [e| let rec xs = case xs of
             []     -> return []
             (y:ys) -> do
               Term t2 v2 <- $(varE self) (y,$(varE g))
               case cmpTy $(varE t1) t2 of
                 Just Equal -> (v2:) `fmap` rec ys
       in  rec |]
