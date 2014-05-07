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
    ( -- * Querying functions
      queryN
    , queryN'
    , queryP
    , queryP'
      -- * Query builder
    , Condition
    , params, (==?), (/=?), (<?), (<=?), (>?), (>=?), (&&?), (||?)
    ) where

import Sound.SC3.Tree.Type

-- --------------------------------------------------------------------------
--
-- Querying functions
--
-- --------------------------------------------------------------------------

-- | Querys given 'SCNode', returns list of 'SCNode' satisfying given condition.
queryN :: Condition SCNode -> SCNode -> [SCNode]
queryN p node =
    let f x acc =
            case x of
                Group _ ns | p x       -> x : foldr f acc ns
                           | otherwise -> foldr f acc ns
                Synth {}   | p x       -> x : acc
                           | otherwise -> acc
    in  foldr f [] [node]

--
-- Variant using Data.Generics.Uniplate.Operations.universe
--
-- queryN p node = [n|n<-universe node, p n]
--


-- | Variant of 'query' returning 'Maybe' value.
queryN' :: Condition SCNode -> SCNode -> Maybe SCNode
queryN' p node = case queryN p node of
    (x:_) -> Just x
    _     -> Nothing

-- | Query given 'SCNode' with conditions to parameters, returns 'SynthParam'
-- satisfying given condition.
queryP :: Condition SynthParam -> SCNode -> [SynthParam]
queryP cond node =
    let f n acc =
            case n of
                Group _ ns   -> foldr f acc ns
                Synth _ _ ps ->
                    let g p acc' = if cond p then p : acc' else acc'
                    in  foldr g acc ps
    in  foldr f [] [node]

-- | Variant of 'queryP' returning 'Maybe' value.
queryP' :: Condition SynthParam -> SCNode -> Maybe SynthParam
queryP' p node = case queryP p node of
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

-- | Lifts '>'.
(>?) :: Ord a => (n -> a) -> a -> Condition n
(>?) = liftQ (>)

-- | Lifts '>='.
(>=?) :: Ord a => (n -> a) -> a -> Condition n
(>=?) = liftQ (>=)

-- | Lifts '<'.
(<?) :: Ord a => (n -> a) -> a -> Condition n
(<?) = liftQ (<)

-- | Lifts '<='.
(<=?) :: Ord a => (n -> a) -> a -> Condition n
(<=?) = liftQ (<=)

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

{-

--- Sample data

nodes :: Nd
nodes =
  grp 0
    [grp 1
      [grp 10
       [mod1, mod2]
      ,grp 11
       [bar1, bar2]]]

mod1, mod2, bar1, bar2 :: Nd
mod1 = syn "foo" ["out"*=100, "amp"*=100, "freq"*=0.66]
mod2 = syn "foo" ["out"*=101, "amp"*=80, "freq"*=3.33]
bar1 = syn "bar" ["amp"*=0.1, "pan"*=0.75, "freq"*=220, "fmod"*<-mod1-*"out"]
bar2 = syn "bar" ["amp"*=0.1, "pan"*=(-0.75), "freq"*=330, "fmod"*<-mod2-*"out"]

q01 :: Maybe SynthParam
q01 = do
    nbar2 <- queryN' (synthName ==? "bar" &&?
                      params (paramValue ==? 101)) (nodify nodes)
    queryP' (paramName ==? "pan") nbar2
-}
