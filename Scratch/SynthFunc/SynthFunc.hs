{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Attempt to make synthdef ugen from function using Template Haskell.
--
-- Suppose there is a function:
--
-- > mySynth :: UGen -> UGen -> UGen
-- > mySynth amp freq = out 0 $ sinOsc ar freq 0 * amp
--
-- And we want to use @control kr "amp" 0.1@ and @control kr "freq" 440@
-- for amp and freq, instead of taking as arguments in the function.
--
-- Desired @mySynth'@ would be:
--
-- > mySynth' :: UGen
-- > mySynth' = out 0 $ sinOsc ar freq 0 * amp
-- >   where
-- >     amp = control kr "amp" 0.1
-- >     freq = control kr "freq" 440
--
-- This module try to make a function which takes a function for making
-- UGen (mySynth in above example) and returns a UGen (mySynth'), with
-- using TemplateHaskell.
--
-- Or even more, how about making [Word8] from mySynth, to send with d_recv?
--
module SynthFunc where

import Data.List (foldl', isPrefixOf)
import Data.Word (Word8)
import Language.Haskell.TH

import Sound.SC3
import Sound.SC3.Lepton

-- What is the type of this function?
--
-- synthFunc :: ??? -> [Word8]
--
synthFunc = undefined

--
-- synthdef "th02" (th02 (control kr  "amp" 0) (control kr "freq" 0) (control kr "pan" 0))
-- == $(sdef [|\amp freq pan -> th02|])
--
sdef = sd2

foo :: UGen
foo = synthFunc foo'

foo' :: UGen -> UGen -> UGen
foo' amp freq = out 0 $ sinOsc ar freq 0 * amp

bar :: UGen
bar = bar' 440

bar' :: UGen -> UGen
bar' freq = out 0 $ sinOsc ar freq 0

barE = letE [] [| bar |]

constant0 :: ExpQ
constant0 = sigE (litE (IntegerL 0)) (conT (mkName "Sound.SC3.UGen.UGen"))

d = [d|func apple = krControlE|]

barDef = [d|bar freq = bar'|]
fooDef = [d|foo amp freq = foo'|]

defs :: Q [Dec]
defs = [d|bar freq = bar';
          foo amp freq = foo'|]

-- | Try:
--
-- > $(sd1 =<< fmap head (runQ th01Def))
--
th01Def = [d|th01 amp freq pan = th01'|]
th01' a f p = out 0 $ pan2 (sinOsc ar f 0 * a) p 1

sd1 def = case def of
  FunD defName [Clause vars (NormalB body) []] -> byte
    where
      byte = foldl appE (varE (mkName "synthdef")) [synthdefName, ugen]
      synthdefName = stringE $ show defName
      ugen = foldl appE func args
      func = return body
      args = map (\(VarP n) -> krControlE (takeWhile (/= '_') (show n))) vars
  _                                            -> error "cannot make synthdef"

-- | Try:
--
-- > withSC3 $ \fd -> async fd $ d_recv $(sd2 [|\amp freq pan -> th02|])
-- > withSC3 $ \fd -> send fd $ s_new "th02" (-1) AddToTail 1 [("amp",0.3),("freq",440)]
--
th02 a f p = out 0 $ pan2 (sinOsc ar f 0 * a * e) p 1
  where
    e = linen (impulse kr 0.1 0) 500e-3 1 8e-3 RemoveSynth

th02Def = [|\amp freq pan -> th02|]
th02Def' = synthdef "th02" $ th02 ("amp"@@0) ("freq"@@0) ("pan"@@0)

-- th02Def'' = sd4 [syn|th02 amp:0.3 freq:3320 pan:0|]
-- syn = undefined
-- sd4 = undefined

-- | Hm, why not ..
th02' = th02 ("amp"@@0) ("freq"@@0) ("pan"@@0)

c0 :: String -> UGen
c0 = (@@ 0)

-- This way is more flexible, since its taking default value for each controls.
th02'' = th02 ("amp"@@0.3) ("freq"@@3320) ("pan"@@0.3)

sd2 exp = sd2' =<< runQ exp

sd2' :: Exp -> ExpQ
sd2' exp = case exp of
  VarE body      -> foldl' appE synthdefE [defNameE body, return exp]
  LamE vars func -> byte
    where
      byte = foldl' appE synthdefE [defNameE func, ugen]
      ugen = foldl' appE (return func) args
      args = map (\(VarP n) -> controlE (show n)) vars
  _              -> error "cannot make synthdef"

synthdefE :: ExpQ
synthdefE = varE (mkName "synthdef")

defNameE :: (Show a) => a -> ExpQ
defNameE name = stringE (tail $ dropWhile (/='.') $ show name)

-- | Pass to different control ugen maker by looking prefix of name.
controlE :: String -> ExpQ
controlE name
  | "a_" `isPrefixOf` name' = arControlE name'
  | "i_" `isPrefixOf` name' = irControlE name'
  | "t_" `isPrefixOf` name' = trControlE name'
  | otherwise               = krControlE name'
  where
    name' = reverse $ tail $ dropWhile (/= '_') (reverse name)

trControlE :: String -> ExpQ
trControlE name = foldl appE v args
  where
    v = varE (mkName "tr_control")
    args = [stringE name, double0]

arControlE :: String -> ExpQ
arControlE = mkAKIControl "ar"

krControlE :: String -> ExpQ
krControlE = mkAKIControl "kr"

irControlE :: String -> ExpQ
irControlE = mkAKIControl "ir"

mkAKIControl :: String -> String -> ExpQ
mkAKIControl rate name = foldl appE v args
  where
    v = varE (mkName "control")
    args = [varE (mkName rate),stringE name, double0]

double0 :: ExpQ
double0 = sigE (litE (IntegerL 0)) (conT (mkName "Double"))

quoteMe :: (String -> String) -> ExpQ
quoteMe f = stringE (f "foo")

nameOne :: Name -> String -> ExpQ
nameOne r n = foldl appE v args
  where
    v = varE (mkName "control")
    args = [varE r, stringE n, double0]

--
-- So .. we still need to write the string literal twice to make the synthdef.
--
-- Then, how about passing a function (UGen -> UGen -> ... -> UGen) and a
-- list [(String,Double)] that containing control name and default value,
-- without using template haskell.
--

-- sd3 :: String -> (a -> UGen) -> [(String,Double)] -> [Word8]
sd3 name func args = synthdef name ugen
  where
    ugen = ugen' func args
    ugen' = undefined

--
-- Fourth attempt, will it be possitlbe to expand:
--
-- > $([expr|foo amp:0.3 freq:440]) = out 0 $ sinOsc ar freq 0 * amp
--
-- to:
--
-- > foo = out 0 $ sinOsc ar freq 0 * amp freq
-- >   where
-- >     amp = control kr "amp" 0.3
-- >     freq = control kr "freq" 440
--

--
-- printf example from ghc users guide
--

-- data Format = D | S | L String

-- parse :: String -> [Format]
-- parse s = [L s]

-- gen :: [Format] -> Q Exp
-- gen [D] = [| \n -> show n |]
-- gen [S] = [| \s -> s |]
-- gen [L s] = stringE s

-- pr :: String -> Q Exp
-- pr s = gen (parse s)

--
-- Quosiquoter example from ghc users guide
--
-- See:
--
-- <http://haskell.org/haskellwiki/Quasiquotation>
--

-- sel :: Int -> Int -> ExpQ
-- sel i n = [| \x -> $(caseE [| x |] [alt]) |]
--   where
--     alt :: MatchQ
--     alt = match pat (normalB rhs) []

--     pat :: PatQ
--     pat = tupP (map varP as)

--     rhs :: ExpQ
--     rhs = varE (as !! (i-1))

--     as :: [Name]
--     as = [mkName $ "a" ++ show a | a <- [1..n]]
