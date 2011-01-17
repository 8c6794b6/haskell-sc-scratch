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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_motors.html>
--
-- Try:
--
-- > > withSC3 motorMachine
--
module Pssd.Engineering.Motors where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Motor sound, take 1.
--
-- Try:
--
-- > > n <- anon motor1
-- > > nset n [("t_trig",1),("curve",-4),("brush",0.0),("rotor",0.2)]
-- > > nset n [("t_trig",1),("top",800),("brush",0.2)]
-- > > nset n [("t_trig",1),("top",130),("optime",1.25)]
--
motor1 :: UGen
motor1 = out2 $ sig * amp
  where
    sig = sig1 * baseEnv
    sig1 = sig2 * cubed (lfSaw ar (baseEnv * top) 0)
    sig2 = (whiteNoise 'a' ar * brush) + rotor
    baseEnv = envGen kr trg 1 0 optime DoNothing $
              env [0,1,0] [0.5,0.5] [curve] (-1) (-1)
    curve = EnvNum (ctrl "curve" (-4))
    trg = ctrl "t_trig" 1
    amp = ctrl "amp" 1
    top = ctrl "top" 500
    optime = ctrl "optime" 3
    brush = ctrl "brush" 0.2
    rotor = ctrl "rotor" 0.2

-- | Motor sound, take 2.
motor2 :: UGen
motor2 = out2 $ sig * amp
  where
    sig = sig1 * baseEnv
    sig1 = lpf (sig2 * cubed (lfSaw ar (baseEnv * top) 0)) 20000
    sig2 = (noise * 0.15 * brush) + rotor
    baseEnv = envGen kr trg 1 0 optime DoNothing $
              env [0,1,0] [0.5,0.5] [curve] (-1) (-1)
    curve = EnvNum (ctrl "curve" (-4))
    noise = mkMetallic n 15 1
    n = hpf (whiteNoise 'a' ar) 100
    trg = ctrl "t_trig" 1
    amp = ctrl "amp" 1
    top = ctrl "top" 500
    optime = ctrl "optime" 3
    brush = ctrl "brush" 0.2
    rotor = ctrl "rotor" 0.2

-- | Metallic filter for noise sound.
mkMetallic :: UGen -- ^ Input noise
           -> UGen -- ^ Reciprocal of Q
           -> UGen -- ^ Frequency factor
           -> UGen
mkMetallic n q f = sum [bpf n (2200*f) (1/q)
                       ,bpf n (3300*f) (1/q)
                       ,bpf n (4200*f) (1/q)
                       ,bpf n (6500*f) (1/q)
                       ,bpf n (9600*f) (1/q)
                       ,bpf n (12000*f) (1/q)] * 0.15

-- | Motor sound, take 3.
--
-- Using fm for getting resonance.
--
motor3 :: UGen
motor3 = out2 $ sig * amp
  where
    sig = (sig1 + metal) * baseEnv
    sig1 = cos ((bpf sig2 top (1/res) + sinOsc ar (top * 1000) 0 * 0.5) * 2 * pi)
    sig2 = clip2 ((metal * brushLevel + rotor) +
                  (cubed $ lfSaw ar (baseEnv * top) 0)) 1
    metal = mkMetallic n 15 brushSize
    n = hpf (whiteNoise 'a' ar) 200
    baseEnv = envGen kr trg 1 0 optime DoNothing $
              env [0,1,0] [0.5,0.5] [curve] (-1) (-1)
    amp = ctrl "amp" 1
    trg = ctrl "t_trig" 1
    curve = EnvNum $ ctrl "curve" (-4)
    brushLevel = ctrl "brushLevel" 3
    brushSize = ctrl "brushSize" 1.5
    top = ctrl "top" 3000
    rotor = ctrl "rotor" 0.1
    optime = ctrl "optime" 1
    res = ctrl "res" 0.5

-- | Control ugen for motors.
turnMotor :: UGen
turnMotor = out outBus val
  where
    outBus = ctrl "out" 120
    val = dust 'd' kr (ctrl "freq" 1)

motorMachine :: (Transport t) => t -> IO ()
motorMachine fd = do
  let dr (n,u) = async fd $ d_recv $ synthdef n u
  mapM_ dr [("motor3",motor3),("turnMotor",turnMotor)]
  mkTree motorGraph fd

motorGraph :: SCNode
motorGraph =
  Group 0
    [Group 1
       [Group 12
         [Group 120
            [Synth 1201 "turnMotor"
               ["out":=121,"freq":=0.12522]
            ,Synth 1202 "turnMotor"
               ["out":=122,"freq":=0.89]
            ,Synth 1203 "turnMotor"
               ["out":=123,"freq":=1.23]
            ,Synth 1204 "turnMotor"
               ["out":=124,"freq":=1.9]]
         ,Group 121
            [Synth 1210 "motor3"
              ["t_trig":<-121,"top":=279,"amp":=0.4,
               "optime":=3.2,"curve":= -2,"res":=0.75,
               "brushSize":=2,"brushLevel":=0.8,"rotor":=0.1]
            ,Synth 1211 "motor3"
              ["t_trig":<-122,"top":=440,"amp":=0.3,
               "optime":=0.75,"curve":= -4,"res":=0.8,
               "brushSize":=0.8,"brushLevel":=1,"rotor":=0.1]
            ,Synth 1212 "motor3"
              ["t_trig":<-123,"top":=932,"amp":=0.3,
               "optime":=0.5,"curve":= -8,"res":=0.9,
               "brushSize":=0.9,"brushLevel":=3,"rotor":=1.3]
            ,Synth 1213 "motor3"
              ["t_trig":<-124,"top":=1650,"amp":=0.3,
               "optime":=0.25,"curve":= -9,"res":=1,
               "brushSize":=1.8, "brushLevel":=2,"rotor":=0.8] ]]]]
