Desired syntax is something like:

foo :: IO UGen
foo = do 
  let s = ug (sinOsc ar)
  constant 0 => phase s
  lfdNoise:r
3 kr 8 => freq s
  return $ out 0 (unUg s)

We want functions like below: 

* Compose s, which understand 'phase', 'freq' and '=>'
* 'phase', which specify phase of s
* 'freq', which specify freq of s
* '=>', which tell that it's left side value will be assigned to attribute 
  specified on right side.

Have no idea how difficult, nor how useful this will be. 

We need a UGen functions.

> import Sound.SC3
> import Sound.SC3.Monadic

sinOsc has type:

  sinOsc :: Rate -> UGen -> UGen -> UGen

its first argument is rate, second is frequency, third is initial phase. Our new
function, sinOsc' has type:

  sinOsc' :: Rate -> ???

where we're not sure about ???, yet. 

.... Nya, anyway.

* We need to define mapping of which argument is accessed by which name?
