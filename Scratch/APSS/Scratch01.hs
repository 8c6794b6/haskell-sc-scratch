{-# LANGUAGE Arrows, DoRec #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (DeriveDataTypeable)

Scratch written while reading:

* Audio Processing and Sound Synthesis in Haskell

-}
module APSS.Scratch01 where

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))
import qualified Prelude

newtype SF a b = SF ([a] -> [b])

instance Category SF where
  SF f . SF g = SF (f . g)
  id = SF id

instance Arrow SF where
  arr f = SF (map f)
  first (SF f) = SF g where
    g l = let (x, y) = unzip l in zip (f x) y

instance ArrowLoop SF where
  loop (SF f) = SF $ \x -> let (y,z) = unzip (f (zip x z)) in y

delay :: a -> SF a a
delay i = SF (i:)

type Sample = Double

-- XXX: Fixed value.
samplingRate = 48000

-- | Delta time between each direct sample.
h = recip samplingRate

sine :: Double -> SF () Sample
sine freq =
  let omh = 2 * pi * freq * h
      d   = sin omh
      c   = 2 * cos omh
      sf  = proc _ -> do
        rec let r = c * d2 - d1
            d2 <- delay d -< r
            d1 <- delay 0 -< d2
        returnA -< r
  in sf

integral :: SF Sample Sample
integral = loop (arr (\(x,i) -> i + h * x) >>>
                 delay 0 >>> arr (\x -> (x, x)))

biOp :: SF a b -> SF c d -> (b -> d -> e) -> SF (a,c) e
biOp a b op = a *** b >>> arr (uncurry op)

(+.) :: SF a Sample -> SF b Sample -> SF (a, b) Sample
a +. b = biOp a b (+)

(*.) :: SF a Sample -> SF b Sample -> SF (a, b) Sample
a *. b = biOp a b (*)

runSF :: SF a b -> [a] -> [b]
runSF (SF f) inp = force (f inp) where
  force []     = []
  force (x:xs) = x `seq` (x : force xs)

mkStream :: SF () Sample -> [Sample]
mkStream ug = runSF ug $ repeat ()

-- Works:
-- ghci> mkStream (arr (const 3))
-- [3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0 ...
--
-- Does not work:
-- ghci> mkStream (sine 110)
--
-- Something went wrong in `sine`. Use of r?
-- It's using d2, which is bounded as result in same line.
--
