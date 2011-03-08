------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Attempt to make synthdef ugen from function using Template Haskell, take 2.
-- 
module SDef where

import Language.Haskell.TH
import Sound.SC3

-- | Suppose we have a function like:
-- 
-- > foo amp freq dur = out 0 sig
-- >   where
-- >     sig = sinOsc ar freq 0 * amp * e
-- >     e = xLine kr 1 1e-9 dur RemoveSynth
-- 
-- And would like to make OSC message for sending synthdef from this function.
-- 
-- > $(sdef 'foo [0.3, 440, 1]) :: OSC
-- 
-- Problem is, how to refer each passed argument with variable used in function 
-- definition? e.g. How to relate 0.3 to "amp", 440 to "freq", and 1 to "dur"?
-- 
-- 
sdef = undefined

fooDef = sdef 'foo 0.3 440 1

foo amp freq dur = out 0 sig
  where
    sig = sinOsc ar freq 0 * amp * e
    e = xLine kr 1 1e-9 dur RemoveSynth
    
-- $(sdef 'foo ["amp"@0.3,"freq"@440,"dur"@1]) = out 0 sig
--   where
--     sig = sinOsc ar freq
--     e   = xLine kr 1 1e-9 dur RemoveSynth

--
-- Declaration splice. 'bar' is 3.
--     
[d|bar = buzz where 
    buzz = buzz2
    buzz2 = 3|]
  
s002 amp freq = out 0 sig
  where
    sig = sinOsc ar freq' 0 * amp'
    amp' = "amp"=:amp
    freq' = "freq"=:freq

-- funD (mkName "blah") [clause [varP $ mkName "amp"] (normalB [|3|]) []]

a = do 
  i <- reify 'foo
  case i of
    VarI n t (Just dec) fxt -> return [dec]
    _                       -> error "pattern match failed" -- return []
    
-- | Defining blah.
$(fmap (:[]) (funD (mkName "blah") [clause [varP $ mkName "x"] (normalB [|3|]) []]))
