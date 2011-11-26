-- | Example written in haddock header.

import Data.Complex
import Data.Array.Repa
import Data.Array.Repa.FFTW

a :: Array DIM1 (Complex Double)
a = fromList (Z :. 5) [i :+ 0 | i <- [0..4]]

-- XXX: Cannot view 'a' when loaded in ghci.
--
-- ghci> a
-- <interactive>:1:1:
--     Can't find interface-file declaration for variable a
--       Probable cause: bug in .hi-boot file, or inconsistent .hi file
--       Use -ddump-if-trace to get an idea of which file caused the error
--     In the expression: a
--     In an equation for `it': it = a
