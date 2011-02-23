{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Example synthdef function.
--
module Foo where

import Language.Haskell.TH

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID

import SynthFunc
import SDef

sendth03 :: (Transport t) => t -> IO ()
sendth03 fd = do
  async fd $ d_recv $(sdef [|\amp freq pan dur -> th03|])
  send fd $ s_new "th03" (-1) AddToTail 1
     [("freq",880),("amp",0.2),("pan",0.3),("dur",2)]

newth03 a f p d fd = do
  send fd $ s_new "th03" (-1) AddToTail 1
    [("amp",a),("freq",f),("pan",p),("dur",d)]

th03 a f p d = mrg [out 0 $ pan2 (sinOsc ar f 0 * a * e) p 1]
  where
    e = decay2 (impulse kr (tfreqe*32) 0) 5e-3 60e-3
    tfreqe = linen (impulse kr 0.001 0) 0.1 1 d RemoveSynth ^ 2

th03' = th03 0.3 440 0.3 2

playth04 :: (Transport t) => t -> IO ()
playth04 fd = do
  async fd $ d_recv $ synthdef "th04" $
    th04 ("t_trig"=:1) ("amp"=:0.3) ("freq"=:3000) ("pan"=:0)
  send fd $ s_new "th04" (-1) AddToTail 1 [("freq",3320)]

th04 t a f p = out 0 $ pan2 (sinOsc ar f 0 * a * e) p 1
  where e = decay2 t 10e-3 300e-3

th05 = out 0 $ sinOsc ar 440 0 * decay2 (impulse kr 1 0) 5e-3 100e-3

th06def = $(sdef [|\i_out amp freq pan -> th06|])
th06 o a f p =
  out o $ pan2 (sinOsc ar f 0 * a * xLine kr 1 1e-9 1 RemoveSynth) p 1

-- \"'rate\" will be expanded and passed as \"Name Foo.rate\" to first
-- argument of \"nameOne\" function.
a = nameOne 'rate "foo"

rate = kr

