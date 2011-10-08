{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

TemplateHaskell helpers.
-}

module Sound.SC3.Lepton.Pattern.Expression.THHelper
  ( -- * Printer
    printQ
  , showQ

    -- * Helper for Deserialier
    -- $deserializer
  , dmatch1
  , dmatch1s
  , dmatch2
  , dmatch2s
  , imatch1
  , imatch2
  , listRec

    -- * Helper for deriving instance
  , derivePint
  , derivePdouble
  , derivePintDup
  , derivePdoubleDup
  ) where

import Data.String
import Language.Haskell.TH

import Data.List.Split

import Sound.SC3.Lepton.Pattern.Expression.Class
import Sound.SC3.Lepton.Pattern.Expression.Term
import Sound.SC3.Lepton.Pattern.Expression.Type

------------------------------------------------------------------------------
-- Helper of helper

-- | Helper orphan instance for adhoc Name.
instance IsString Name where
  fromString = mkName

-- | Show contents after applying ppr.
printQ :: Ppr a => Q a -> IO ()
printQ x = putStrLn =<< (runQ $ (show . ppr) `fmap` x)

-- | Show raw TH data.
showQ :: Show a => Q a -> IO ()
showQ x = putStrLn =<< (runQ $ show `fmap` x)

-- | Extract last part of name.
baseName :: Show a => a -> ExpQ
baseName n = litE (stringL . last . splitOn "." . show $ n)

------------------------------------------------------------------------------
-- For deserializer

{-$deserializer

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
               _          -> Left $ "match1: type mismatch"
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
               _                       -> Left "match2: type mismatch"
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
                 Nothing    -> error "listRec: type mismatch"
       in  rec |]

------------------------------------------------------------------------------
-- Instance deriving helpers

{-|

Helper for derining instance of Pint type class.

> instance Pint 'tname where
>   pint x = 'fint "pint" x
>   (+!) x y  = 'fbinary "+!" x y
>   (*!) x y  = 'fbinary "*!" x y
>   ...
>   pinegate x = 'funary "pinegate" x
>   piabs x = 'funary "piabs" x
>   ...

-}
derivePint ::
  Name -- ^ tname
  -> Name -- ^ fint
  -> Name -- ^ funary
  -> Name -- ^ fbinary
  -> Q [Dec]
derivePint tname fint funary fbinary = instanceD' contexts def decs
  where
    instanceD' x y z = fmap (:[]) (instanceD x y z)
    contexts   = cxt []
    def        = appT (conT ''Pint) [t| $(conT tname)|]
    decs       = [intdef] ++ unaryDefs ++ binaryDefs
    intdef     = mkprimf 'pint fint
    unaryDefs  = mkunaryf funary ['pinegate, 'piabs, 'pisignum]
    binaryDefs = mkbinaryf fbinary ['(+!), '(*!), '(-!), 'pirange]

{-|

Helper for deriving instance of Pdouble type class.

> instance Pdouble 'tname where
>   pdouble x = 'fdouble "pint" x
>   (+@) x y  = 'fbinary "+@" x y
>   (*@) x y  = 'fbinary "*@" x y
>   ...
>   pdnegate x = 'funary "pinegate" x
>   pdabs x = 'funary "piabs" x
>   ...
>   ppi = 'fpi "ppi"

-}
derivePdouble ::
  Name -- ^ tname
  -> Name -- ^ fdouble
  -> Name -- ^ fpi
  -> Name -- ^ funary
  -> Name -- ^ fbinary
  -> Q [Dec]
derivePdouble tname fdouble fpi funary fbinary = instanceD' contexts def decs
  where
    instanceD' x y z = fmap (:[]) (instanceD x y z)
    contexts   = cxt []
    def        = appT (conT ''Pdouble) [t| $(conT tname)|]
    decs       = [doubledef,pidef] ++ unaryDefs ++ binaryDefs
    doubledef  = mkprimf 'pdouble fdouble
    pidef      = funD 'ppi [clause [] (normalB [| $(varE fpi) |]) []]
    unaryDefs  = mkunaryf funary df1s
    binaryDefs = mkbinaryf fbinary df2s

-- | Functions in Pdouble which takes single argument.
df1s =
  -- XXX: Is there a way to extract these functions?
  [ 'pdnegate, 'pdabs, 'pdsignum, 'precip, 'pexp, 'psqrt, 'plog
  , 'psin, 'ptan, 'pcos, 'pasin, 'patan, 'pacos, 'psinh, 'ptanh, 'pcosh
  , 'pasinh, 'patanh, 'pacosh, 'pampDb, 'pasFloat, 'pasInt, 'pbitNot
  , 'pcpsMIDI, 'pcpsOct, 'pcubed, 'pdbAmp, 'pdistort, 'pfrac, 'pisNil
  , 'plog10, 'plog2, 'pmidiCPS, 'pmidiRatio, 'pnotE, 'pnotNil, 'poctCPS
  , 'pramp_, 'pratioMIDI, 'psoftClip, 'psquared ]

-- | Functions in Pdouble which takes 2 arguments.
df2s = ['(+@), '(*@), '(-@), 'pdrange, '(/@), '(**@), 'plogBase]

-- | Helper to build prim function.
mkprimf :: Name -> Name -> DecQ
mkprimf ftarget fobject =
  funD ftarget
  [clause [varP "x"]
   (normalB [| $(varE fobject) $(baseName ftarget) $(varE "x") |]) []]

-- | Helper to build single arg function.
mkunaryf :: Name -> [Name] -> [DecQ]
mkunaryf funary fns = map g fns where
  g fn =
    funD fn [clause [varP "x"]
             (normalB [| $(varE funary) $(baseName fn) $(varE "x") |]) []]

-- | Helper to build function taking 2 args.
mkbinaryf :: Name -> [Name] -> [DecQ]
mkbinaryf fbinary fns = map g fns where
  g fn =
    funD fn
    [clause [varP "x", varP "y"]
     (normalB [| $(varE fbinary) $(baseName fn) $(varE "x") $(varE "y") |]) []]

-- | Helper for Dup data type to derive instance of Pint.
derivePintDup :: Name -> Name -> Name -> Name -> Q [Dec]
derivePintDup tname fint funary fbinary = instanceD' contexts def decs
  where
    instanceD' x y z = fmap (:[]) (instanceD x y z)
    contexts   = cxt [classP ''Pint [varT "l"], classP ''Pint [varT "r"]]
    def = appT (conT ''Pint) [t| $(conT tname) $(varT "l") $(varT "r")|]
    decs = [intdef] ++ unaryDefs ++ binaryDefs
    intdef =
      funD 'pint
      [clause [varP "x"] (normalB [| $(varE fint) $(varE "x") |]) []]
    unaryDefs  = flip map ['pinegate, 'piabs, 'pisignum] $ \f ->
      funD f [clause [varP "x"]
              (normalB [| $(varE funary) $(varE f) $(varE "x") |]) []]
    binaryDefs = flip map ['(+!), '(*!), '(-!), 'pirange] $ \f ->
      funD f [clause [varP "x", varP "y"]
       (normalB [| $(varE fbinary) $(varE f) $(varE "x") $(varE "y") |]) []]

-- | Helper for Dup data type to derive instance of Pdouble.
derivePdoubleDup :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
derivePdoubleDup tname fdouble fpi funary fbinary = instanceD' contexts def decs
  where
    instanceD' x y z = fmap (:[]) (instanceD x y z)
    contexts   = cxt [classP ''Pdouble [varT "l"], classP ''Pdouble [varT "r"]]
    def        = appT (conT ''Pdouble) [t| $(conT tname) $(varT "l") $(varT "r")|]
    decs       = [doubledef,pidef] ++ unaryDefs ++ binaryDefs
    doubledef  =
      funD 'pdouble
      [clause [varP "x"] (normalB [| $(varE fdouble) $(varE "x") |]) []]
    pidef      = funD 'ppi [clause [] (normalB [| $(varE fpi) |]) []]
    unaryDefs  = map mkU df1s
    mkU f      =
      funD f
      [clause [varP "x"]
       (normalB [|$(varE funary) $(varE f) $(varE "x")|]) []]
    binaryDefs = map mkB df2s
    mkB f      =
      funD f
      [clause [varP "x", varP "y"]
       (normalB [| $(varE fbinary) $(varE f) $(varE "x") $(varE "y") |]) []]
