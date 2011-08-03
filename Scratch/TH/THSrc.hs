{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to ghc)
--
-- Playing with Template Haskell, printf.
--
module THSrc where

import Language.Haskell.TH

printf :: String -> ExpQ
printf s = gen (parse s) [| "" |]

data Format = D | S | L String
            deriving (Eq, Show)

parse :: String -> [Format]
parse str = case str of
  ""           -> []
  '%':'d':rest -> D : parse rest
  '%':'s':rest -> S : parse rest
  _            -> L l : parse r where (l,r) = span (/= '%') str

gen :: [Format] -> ExpQ -> ExpQ
gen xs x = case xs of
  []      -> x
  D:xs'   -> [| \n -> $(gen xs' [| $x ++ show n |]) |]
  S:xs'   -> [| \s -> $(gen xs' [| $x ++ s |]) |]
  L s:xs' -> gen xs' [| $x ++ $(stringE s) |]

sel :: (Num t, Enum t) => Int -> t -> ExpQ
sel i n = lamE [pat] rhs
  where
    pat = tupP (map varP as)
    rhs = varE (as !! (i-1))
    as = [mkName $ "a" ++ show j | j <- [1..n]]

sel' :: Int -> Int -> ExpQ
sel' i n = [| \x -> $(caseE [| x |] [alt]) |]
  where
    alt = match pat (normalB rhs) []
    rhs = varE (as !! (i-1))
    pat = tupP (map varP as)
    as = [mkName $ "a" ++ show j | j <- [1..n]]

-- | To see pretty printed representation, try:
--
-- > > ppr `fmap` (runQ $ zipN 3)
--
zipN :: Int -> ExpQ
zipN n = [|let z = $(mkZip n [|z|]) in z|]

mkZip :: Int -> ExpQ -> ExpQ
mkZip n self = lamE pYs (caseE (tupE eYs) [m1,m2])
  where
    (pXs, eXs) = genPE "x" n
    (pYs, eYs) = genPE "y" n
    (pXSs, eXSs) = genPE "xs" n
    m1 = match (tupP (zipWith pcons pXs pXSs)) (normalB b) []
    m2 = match (tupP (replicate n wildP)) (normalB [|[]|]) []
    pcons x xs = infixP x '(:) xs
    b = [|$(tupE eXs) : $(apps (self:eXSs))|]

    -- b = infixE (Just $ tupE eXs) (conE '(:)) (Just $ apps (self:eXSs))
    -- m2 = match (tupP (replicate n wildP)) (normalB (listE [])) []

apps :: [ExpQ] -> ExpQ
apps [x] = x
apps (x:y:zs) = apps ([| $x $y |] : zs)

genPE :: String -> Int -> ([PatQ], [ExpQ])
genPE s n = let ns = [mkName $ s ++ (show i) | i <- [1..n]]
            in  (map varP ns, map varE ns)

cross2a :: ExpQ -> ExpQ -> ExpQ
cross2a f g = [| \(x, y) -> ($f x, $g y) |]

cross2b :: ExpQ -> ExpQ -> ExpQ
cross2b f g =
  lamE [tupP [varP $ mkName "x", varP $ mkName "y"]]
  (tupE [appE f (varE $ mkName "x"), (appE g (varE $ mkName "y"))])

cross2c :: ExpQ -> ExpQ -> ExpQ
cross2c f g = do
  x <- newName "x"
  y <- newName "y"
  f' <- f
  g' <- g
  return $
    LamE [TupP [VarP x, VarP y]]
    (TupE [AppE f' (VarE x)
          ,AppE g' (VarE y)])

cross2d :: ExpQ -> ExpQ -> ExpQ
cross2d f g = do
  x <- newName "x"
  y <- newName "y"
  lamE [tupP [varP x, varP y]]
    (tupE [appE f (varE x)
          ,appE g (varE y)])

genpat :: PatQ -> Q (String -> ExpQ, PatQ)
genpat p = case p of
  _ -> undefined

infoExample :: String -> ExpQ
infoExample n = do
  info <- reify (mkName n)
  case info of
    ClassI _ _ -> stringE "Class"
    ClassOpI _ _ _ _ -> stringE "ClassOp"
    TyConI d -> stringE $ unwords ["TyCon:", show d]
    PrimTyConI _ _ _ -> stringE "PrimTyCon"
    DataConI n1 t n2 f ->
      stringE $ unwords ["DataCon:", show n1, show n2]
    VarI n t d f ->
      stringE $ unwords ["Var:", show n, show t, show d]
    TyVarI _ _ -> stringE "tyVar"
