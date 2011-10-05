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
module Scratch.THHelper where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi)

import Scratch.PC02
import Scratch.Term00
import Scratch.Type00
import Scratch.Etree

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

-- ghci> printQ $ classP ''Num [varT (mkName "a")]
-- GHC.Num.Num a

-- ghci> printQ $ tySynD (mkName "Foo") [PlainTV (mkName "a")] (varT (mkName "a"))
-- type Foo a = a

-- ghci> printQ $ (\a -> [t| (Num $a) => Maybe $a |]) (varT (mkName "a"))
-- forall . GHC.Num.Num a => Data.Maybe.Maybe a

-- ghci> printQ [d| type Foo a = (Num a) => Maybe a; type Bar b = Monad m => m b |]
-- type Foo a_0 = forall . GHC.Num.Num a_0 => Data.Maybe.Maybe a_0
-- type Bar b_1 = forall m_2 . GHC.Base.Monad m_2 => m_2 b_1

ftType =
  [d| type FromTree r = forall g h.
           (Pint r, Pdouble r)
           => (Etree,g) -> Either String (Term r h) |]

ftlType =
  [d| type FromTreeList r = forall g h.
           (Pint r, Pdouble r)
           => forall t. Ty t -> ([Etree],g) -> Either String [r h t] |]

ftlType3 =
  [d| type FromTreeList r = forall g h. (Pint r)
           => forall t. Ty t -> ([Etree],g) -> Either String [r h t] |]

{-|
What we want to do is, produce a class constraints like:

> ( Pint r, Pdouble r, Pappend r, Pconcat r
> , Preplicate r, Pseq r, Pforever r,Pcycle r
> , Prand r, Pshuffle r, Ptuple r
> , Plambda r, VarEnv g h
> , Psnew r, Pmerge r, Ppar r
> ) => ...

Here, type variable r, g, and h are used. We want to reuse these
variables in the body expression of type signature, written as '...'
above, like:

> type FromTree r =
>   forall g h.
>   ( Pint r, Pdouble r, Pappend r, Pconcat r
>   , Preplicate r, Pseq r, Pforever r,Pcycle r
>   , Prand r, Pshuffle r, Ptuple r
>   , Plambda r, VarEnv g h
>   , Psnew r, Pmerge r, Ppar r
>   ) => (Etree,g) -> Either String (Term r h)

-}
ftTypes :: String -> String -> Q [Dec]
ftTypes n1 n2 = do
  let r = mkName "r"; g = mkName "g"; h = mkName "h"; v = varT
  c <- tySynD (mkName n1) [PlainTV r]
       (forallT [PlainTV g, PlainTV h] (preds r g h)
        ([t| (Etree,$(v g)) -> Either String (Term $(v r) $(v h))|]))
  d <- tySynD (mkName n2) [PlainTV r]
       (forallT [PlainTV g, PlainTV h] (preds r g h)
        ([t| forall t.
             $(appT (conT (mkName n1)) (v r)) -> Ty t ->
              ([Etree],$(v g)) -> Either String [$(v r) $(v h) t]|]))
  return [c,d]

-- | Class constraints used in ftTypes.
preds :: Name -> Name -> Name -> CxtQ
preds r g h =
  let r' = varT r; f c = classP c [r']
  in  cxt (classP ''VarEnv [varT g, varT h]: map f patternClasses)

-- | List of pattern expression classes.
patternClasses :: [Name]
patternClasses =
  [ ''Pint, ''Pdouble
  , ''Pappend, ''Pconcat, ''Preplicate, ''Pseq, ''Pforever, ''Pcycle
  , ''Prand, ''Pshuffle
  , ''Plambda, ''Ptuple
  , ''Psnew, ''Pmerge, ''Ppar
  ]
