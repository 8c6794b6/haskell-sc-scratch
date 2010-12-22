------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Rain
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Rain>
--
-- /Example/:
--
--
module DesigningSound.Rain where

import Sound.SC3
import Sound.SC3.ID
import qualified Sound.SC3.Monadic as M

-- | Fluid sphere impact
--
-- > ~dropletonhard = {|dur=0.0005| Env.perc(dur/2, dur/2, 1, [-4, 4])};
--
dropletOnHard :: UGen -> [UGen]
dropletOnHard dur = env [0, 1, 0] [dur/2, dur/2] [EnvNum (-4), EnvNum 4] (-1) 1

-- | Obtaining a Gaussian noise distribution
--
-- > x = { {WhiteNoise.ar}.dup(12).mean.dup * 0.5}.play;
--
gaussian1 :: UGen
gaussian1 = out 0 $(sum (map mkNoise [1..12]) / 12) * 0.5
  where
    mkNoise :: Int -> UGen
    mkNoise i = whiteNoise i ar

-- | Another gaussian
--
-- > x = {
-- >    var amount=MouseX.kr(0,1); // move mouse left/right to change amount
-- >    var n1 = WhiteNoise.ar.abs * amount + (1-amount);
-- >    var n2 = WhiteNoise.ar(2pi);
-- >    var gaussian = sqrt(-2 * log(n1)) * cos(n2);
-- >    gaussian.dup * 0.5
-- > }.play;
--
gaussian2 :: IO UGen
gaussian2 = do
    let amount = mouseX kr 0 1 Linear 0.01
    n1 <- (\x -> abs x * amount + (1-amount)) `fmap` M.whiteNoise ar
    n2 <- (* (2*pi)) `fmap` M.whiteNoise ar
    let gaussian = sqrt (-2 * log n1) * cos n2
    sig <- clone 2 (return gaussian)
    return $ out 0 (mce [sig, sig] * 0.2)

-- | Raindrop pressure on solid ground
--
-- > x = {
-- >    var gaus, osc;
-- >    gaus = {WhiteNoise.ar}.dup(12).sum;
-- >    gaus = LPF.ar(BPF.ar(gaus, 50, 1/0.4), 500);
-- >
-- >    //
-- >    osc = SinOsc.ar(gaus.linlin(-1, 1, 40, 80)) * gaus.squared * 10;
-- >    osc = (osc - 0.35).max(0);
-- >
-- >    2.do{
-- >                          osc = HPF.ar(osc, 500);
-- >    };
-- >
-- >    osc.dup
-- > }.play
--
solidGround :: UGen
solidGround = out 0 (mce2 sig1 sig1)
  where
    sig1 = foldl hpf sig2 [500, 500]
    sig2 = max (sig3 - 0.35) 0
    sig3 = sinOsc ar (linLin gaus (-1) 1 40 80) 0 * squared gaus * 10
    gaus = lpf (bpf gaus' 50 (1/0.4)) 500
    gaus' = sum (map mkNoise [1..12::Int])
    mkNoise i = whiteNoise i ar
