module SCHelp.PG.Cookbook01 where

import Control.Applicative

import FRP.Reactive
import Sound.SC3
import Sound.SC3.Lang.Math
import Sound.OpenSoundControl

import Reusable
import SCQuery
import SCTree
import SCSched
import Scratch.Pat.P5
import qualified Scratch.ControlArgs as A

fuguePitch :: Pitch Double
fuguePitch = defaultPitch { scale = [0, 2, 3, 5, 7, 8, 10], root = 2}

degreeToFreq d = toPv $ freq $ fuguePitch {degree=d}

degrees = [3,2,1,0,-0.9,0,1,2,-3,-1.9,-0.9,0,-0.9,0,1,2]

sandwitch :: a -> [a] -> [a]
sandwitch _ [] = []
sandwitch a (x1:xs) = x1 : a : sandwitch a xs

sandwitchedDegrees :: [Double]
sandwitchedDegrees = 4 : sandwitch 4 degrees

fugue :: Pattern PValue
fugue =
    Pbind
    [("freq",Pseq 1 $ map (Pid . degreeToFreq) sandwitchedDegrees),
     ("dur", Pseq 1 $ take (length sandwitchedDegrees) $
           repeat (Pid (PNum 0.25)))]

prepareListE :: String -> [[(String,Double)]] -> [(Double,OSC)]
prepareListE name xs = fst $ foldr f v xs
    where f xs (xs',s) = (xs' ++ [(s',osc)], s')
              where
                osc = s_new name (-1) AddToTail 1 xs
                s' = s + (maybe 0 id $ lookup "dur" xs)
          v = ([],0)

runFugue = do
  params <- map fromPv <$> runPIO fugue
  return $ listE $ prepareListE "simpleSynth" params

simpleSynth :: UGen
simpleSynth = out 0 $ pan2 (sinOsc ar A.freq 0 * env) 0 1 * 0.3
    where env = envGen kr 1 1 0 1 RemoveSynth (envPerc 0.1 0.5)
