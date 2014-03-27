{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : unknown

Functions to query 'SCNode' with conditions.

-}
module Sound.SC3.Tree.Query
    ( -- * Example
      -- $example

      -- * Querying functions
      queryN
    , queryN'
      -- * Query builder
    , Condition
    , params, (==?), (/=?), (&&?), (||?)
    ) where

import Data.Generics.Uniplate.Data (universe)

import Sound.SC3.Tree.Type

{-$example



-}


-- --------------------------------------------------------------------------
--
-- Querying functions
--
-- --------------------------------------------------------------------------

-- | Querys given 'SCNode', returns list of 'SCNode' satisfying given condition.
queryN :: Condition SCNode -> SCNode -> [SCNode]
queryN p node = [n|n<-universe node, p n]

-- | Variant of 'query' returning 'Maybe' value.
queryN' :: Condition SCNode -> SCNode -> Maybe SCNode
queryN' p node = case queryN p node of
    (x:_) -> Just x
    _     -> Nothing


-- --------------------------------------------------------------------------
--
-- Query builders
--
-- --------------------------------------------------------------------------


-- | A function to filter out matching elements.
type Condition a = a -> Bool

-- | Converts condition for synth parameters to condition for 'SCNode'.
params :: Condition SynthParam -> Condition SCNode
params f n = case n of
    Synth _ _ ps -> any f ps
    _            -> False

-- | Lifts '=='.
(==?) :: Eq a => (n -> a) -> a -> Condition n
(==?) = liftQ (==)

-- | Lifts '/='.
(/=?) :: Eq a => (n -> a) -> a -> Condition n
(/=?) = liftQ (/=)

-- | Lifts '&&'
(&&?) :: Condition n -> Condition n -> Condition n
(&&?) = liftQ2 (&&)

-- | Lifts '||'.
(||?) :: Condition n -> Condition n -> Condition n
(||?) = liftQ2 (||)

infix 4 ==?
infix 4 /=?

infixr 3 &&?
infixr 2 ||?


-- --------------------------------------------------------------------------
--
-- Helpers
--
-- --------------------------------------------------------------------------

liftQ :: (d -> a -> c) -> (b -> d) -> a -> b -> c
liftQ op f v = \x -> f x `op` v

liftQ2 :: (c -> d -> b) -> (a -> c) -> (a -> d) -> a -> b
liftQ2 op f g v = f v `op` g v
