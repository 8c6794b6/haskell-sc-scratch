{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Representation of scsynth node tree.
--
module Sound.SC3.Lepton.Tree
  ( -- * Examples
    -- $examples

    -- * Types
    SCTree(..)
  , NodeId
  , SynthName
  , SynthParam(..)
  , ParamName
  , ParamValue
  , BusId

    -- * Parser
  , DatumParser(..)
  , parseOSC

    -- * Converter
  , treeToOSC
  , paramToTuple
  , drawSCTree

    -- * Communicating with server
  , getTree
  , mkTree
  , printTree
  )  where

import Control.Monad
import Data.Generics (Data, Typeable)
import Data.Tree
import Text.Printf (printf)

import Sound.SC3
import Sound.OpenSoundControl

import Sound.SC3.Lepton.Util (queryTree)
import Sound.SC3.Lepton.Instance ()

-- $examples
--
-- Dump the contents of running synth nodes.
--
-- > > :m + Sound.SC3
-- > > withSC3 reset
-- > > withSC3 printTree
-- > Group 0
-- > `-Group 1
-- > > audition $ out 0 $ sinOsc ar (control kr "freq" 440) 0 * 0.3
-- > > audition $ out 0 $ sinOsc ar (control kr "freq" 330) 0 * 0.3
-- > > withSC3 printTree
-- > Group 0
-- > `-Group 1
-- >   +-Synth -48 Anonymous
-- >   | `-[freq := 440.00]
-- >   `-Synth -56 Anonymous
-- >     `-[freq := 330.00]
--
-- Changing synthdef name with using @everywhere@ from syb. After above, run:
--
-- > > :m + Data.Generics
-- > > t <- withSC3 getTree
-- > > withSC3 reset
-- > > let f (Synth i n ps) = Synth (1000 + abs i) "default" ps; f x = x
-- > > withSC3 $ mkTree $ everywhere (mkT f) t
--
-- Or using @transform@ from uniplate:
--
-- > > :m + Data.Generics.Uniplate.Data
-- > > t <- withSC3 getTree
-- > > let g (Synth i n ps) = Synth (2000 + abs i) "default" ps; g x = x
-- > > withSC3 $ mkTree $ transform g t
--

------------------------------------------------------------------------------
--
-- Types
--
------------------------------------------------------------------------------

-- | Data type for representing Group and Synth node in scsynth.
data SCTree = Group NodeId [SCTree]                -- ^ Group node
            | Synth NodeId SynthName [SynthParam]  -- ^ Synth node
              deriving (Eq,Read,Show,Data,Typeable)

type NodeId = Int
type SynthName = String

-- | Data type for synth param.
data SynthParam = ParamName := ParamValue -- ^ Double value
                | ParamName :<- BusId     -- ^ Mapped control bus id
                  deriving (Eq,Read,Data,Typeable)

instance Show SynthParam where
  show (f := x)  = f ++ " := " ++ printf "%.2f" x
  show (f :<- x) = f ++ " :<- " ++ show x

type ParamName = String
type ParamValue = Double
type BusId = Int

infixr 5 :=
infixr 5 :<-

------------------------------------------------------------------------------
--
-- Parser
--
------------------------------------------------------------------------------

-- | Parser for datum.
newtype DatumParser a = DatumParser {parse::[Datum] -> [(a,[Datum])]}

instance Monad DatumParser where
    return a = DatumParser $ \ds -> [(a,ds)]
    p >>= f = DatumParser $ \cs ->
              concat [parse (f a) cs' | (a,cs') <- parse p cs]

instance MonadPlus DatumParser where
    mzero = DatumParser $ \_ -> []
    p `mplus` q = DatumParser $ \cs -> parse p cs ++ parse q cs

manyN :: Int -> DatumParser a -> DatumParser [a]
manyN 0 _ = return []
manyN n p = do
  x <- p
  xs <- manyN (n-1) p
  return (x:xs)

datum :: DatumParser Datum
datum = DatumParser $ \cs ->
       case cs of
         []     -> []
         (d:ds) -> [(d,ds)]

int :: DatumParser Int
int = do {d <- datum; case d of {Int x -> return x; _ -> mzero}}

double :: DatumParser Double
double = do {d <- datum; case d of {Double x -> return x; _ -> mzero}}

float :: DatumParser Double
float = do {d <- datum; case d of {Float x -> return x; _ -> mzero}}

string :: DatumParser String
string = do {d <- datum; case d of {String x -> return x; _ -> mzero}}

-- | Parse osc message returned from \"/g_queryTree\" and returns haskell
-- representation of scsynth node tree.
-- Only working with osc message including synth control parameters.
parseOSC :: OSC -> SCTree
parseOSC (Message s ds)
    | s == "/g_queryTree.reply" = parseDatum ds
    | otherwise                 = error "not a /g_queryTree.reply message"
parseOSC _ = error "not a Message response"

parseDatum :: [Datum] -> SCTree
parseDatum ds = fst $ head $ parse parseGroup $ tail ds

parseGroup :: DatumParser SCTree
parseGroup = do
  nId <- int
  numChild <- int
  if numChild < 0
    then parseSynth nId
    else do
      ts <- manyN numChild parseGroup
      return $ Group nId ts

parseSynth :: Int -> DatumParser SCTree
parseSynth nId = do
  name <- string
  numParams <- int
  params <- manyN numParams parseParam
  return $ Synth nId name params

parseParam :: DatumParser SynthParam
parseParam = do
    name <- string
    val <- parseParamValue name
    return val

parseParamValue :: String -> DatumParser SynthParam
parseParamValue name = do
  val <- datum
  case val of
    Float x  -> return $ name := x
    Double x -> return $ name := x
    String x -> return $ name :<- (read $ tail x)
    Int x    -> return $ name := fromIntegral x
    e        -> error $ "Cannot make param from: " ++ show e

------------------------------------------------------------------------------
--
-- Converting functions
--
------------------------------------------------------------------------------

-- | SCTree to [OSC].
--
-- OSC list contains \"g_new\", \"s_new\", and \"n_map\" messages to build
-- the given SCTree.
treeToOSC :: SCTree -> [OSC]
treeToOSC tree = tail $ f 0 tree
  where
    f i t = case t of
      Group j ns   -> g_new [(j,AddToTail,i)]:concatMap (f j) ns
      Synth j n ps -> s_new n j AddToTail i (concatMap paramToTuple ps):g j ps
    g i ps | not $ null qs = [n_map i qs]
           | otherwise     = []
      where qs = foldr h [] ps
    h a b = case a of
      (name:<-bus) -> (name,bus):b
      _            -> b

paramToTuple :: SynthParam -> [(String,Double)]
paramToTuple (name := val) = [(name,val)]
paramToTuple _ = []

paramToMap :: NodeId -> SynthParam -> [OSC]
paramToMap i (n :<- b) = [n_map i [(n,b)]]
paramToMap _ _ = []

-- | For converting SCTree to Tree datatype in Data.Tree.Tree.
-- Data.Tree.Tree is a rose tree, but SCTree datatype is not.
type SCTree' = Tree SCNode

-- | Wrapper for SCTree.
data SCNode = G NodeId
            | S NodeId SynthName
            | P [SynthParam]
              deriving (Eq)

instance Show SCNode where
    show (G nid) = "Group " ++ show nid
    show (S nid name) = "Synth " ++ show nid ++ " " ++ name
    show (P ps) = show ps

-- | Draw SCTree data.
drawSCTree :: SCTree -> String
drawSCTree = unlines . draw . fmap show . toRose

toRose :: SCTree -> SCTree'
toRose (Group nid ns)   = Node (G nid) (map toRose ns)
toRose (Synth nid name ps)
  | null ps   = Node (S nid name) []
  | otherwise = Node (S nid name) [Node (P ps) []]

draw :: Tree String -> [String]
draw (Node x ts0) = x:drawSubTrees ts0
  where
    drawSubTrees []     = []
    drawSubTrees (t:[]) = shift "`-" "  " (draw t)
    drawSubTrees (t:ts) = shift "+-" "| " (draw t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first:repeat other)

------------------------------------------------------------------------------
--
-- Communicating with server
--
------------------------------------------------------------------------------

-- | Get current node mapping representation of @SCTree@.
getTree :: (Transport t) => t -> IO SCTree
getTree fd = queryTree fd >>= return . parseOSC

-- | Send OSC message for constructing given @SCTree@.
mkTree :: (Transport t) => SCTree -> t -> IO ()
mkTree t = \fd -> do
  t0 <- utcr
  send fd (Bundle (UTCr (t0 + 0.1)) (treeToOSC t))

-- | Prints current SCTree.
printTree :: Transport t => t -> IO ()
printTree fd = getTree fd >>= putStr . drawSCTree
