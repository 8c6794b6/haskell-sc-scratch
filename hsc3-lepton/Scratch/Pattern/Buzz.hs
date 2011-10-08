{-# LANGUAGE NoMonomorphismRestriction #-}
module Scratch.Pattern.Buzz where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

main :: IO ()
main = withSC3 goBuzz

-- | Play the pattern.
goBuzz :: Transport t => t -> IO ()
goBuzz fd = do
  async fd $ d_recv $ synthdef "buzz" buzz
  play fd $ toL pBuzz

-- | UGen for buzz.
buzz :: UGen
buzz = out 0 $ pan2 sig pan 1
  where
    sig = sinOsc AR freq 0 * amp * e
    e = linen tr 5e-3 1 (10e-3+(220/freq)) RemoveSynth ^ 2
    amp = control KR "amp" 0.3
    freq = control KR "freq" 440
    pan = control KR "pan" 0
    tr = tr_control "t_trig" 1

-- Pattern for amp, dur, freq, and pan.
pBuzz = psnew "buzz" Nothing AddToTail 1
  [("amp",
    pcycle (ds [0.3, 0.1,  0.1,   0.3,  0.1,  0.1,  0.1]))
  ,("dur",
    pcycle (ds [1,   0.55, 0.45,  0.54, 0.46, 0.53, 0.47]) *@
    pforever (d (60/160)))
  ,("freq",
    pmidiCPS $ pcycle
    [d 48, pr 13 cm, d 53, pr 13 fm
    ,d 48, pr 13 cm, d 43, pr 13 g7
    ,d 48, pr 13 cm, d 53, pr 13 fm
    ,d 50, pr 6 fm, d 43, pr 6 g7
    ,d 48, pr 6 cm, d 55, pr 6 cm])
  ,("pan",
    pcycle [pconcat (ds [-1,-0.9..1]),pconcat (ds [1,0.9..(-1)])])]
  where
    cm = ds [55, 67,72,75,79,84,87]
    fm = ds [60, 68,72,77,80,84,89]
    g7 = ds [50, 67,71,74,77,79,83]
    i = pint; d = pdouble; ds = map pdouble; pr x = prand (i x)
