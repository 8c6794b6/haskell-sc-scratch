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
-- /Example/:
--
-- > > withSC3 printTree
-- > > t <- withSC3 getTree
-- > > print $ treeToOSC t
--
module Sound.SC3.Lepton.Tree
  ( -- * Types
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

    -- * Communicating with server
  , getTree
  , mkTree
  , printTree
  , queryAllNodes
  )  where

import Control.Monad
import Data.Generics (Data, Typeable)
import Data.Tree
import Text.Printf (printf)

import Sound.SC3
import Sound.OpenSoundControl

import Sound.SC3.Lepton.Util (queryTree)
import Sound.SC3.Lepton.Instance ()

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
-- Implementation is done with recursing by hand.
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

------------------------------------------------------------------------------
--
-- Communicating with server
--
------------------------------------------------------------------------------

-- | Get current node mapping representation of @SCTree@.
getTree :: (Transport t) => t -> IO SCTree
getTree fd = queryTree fd >>= return . parseOSC

-- | Send node mapping OSC message scsynth, defined by @SCTree@.
mkTree :: (Transport t) => SCTree -> t -> IO ()
mkTree t = \fd -> do
  t0 <- utcr
  send fd (Bundle (UTCr (t0 + 0.1)) (treeToOSC t))

-- | For converting SCTree to Tree datatype in Data.Tree.Tree.
-- Data.Tree.Tree is a rose tree, but SCTree datatype is not.
type SCTree' = Tree SCNode

-- | Wrapper for SCTree.
data SCNode = G NodeId
            | S NodeId SynthName [SynthParam]
              deriving (Eq)

instance Show SCNode where
    show (G nid) = "Group " ++ show nid
    show (S nid name ps) = "Synth " ++ show nid ++ " " ++ name ++ " " ++ show ps

toRose :: SCTree -> SCTree'
toRose (Group nid ns)      = Node (G nid) (map toRose ns)
toRose (Synth nid name ps) = Node (S nid name ps) []

drawSCTree :: SCTree -> String
drawSCTree = drawTree . fmap show . toRose

-- | Prints current SCTree.
printTree :: Transport t => t -> IO ()
printTree fd = getTree fd >>= putStr . drawSCTree

-- | Alias to @printTree@.
queryAllNodes :: Transport t => t -> IO ()
queryAllNodes = \fd -> getTree fd >>= putStr . drawSCTree

------------------------------------------------------------------------------
--
-- Scratch
--
------------------------------------------------------------------------------

-- | Sample message returned from scsynth server, without group other
-- than default.
oscList1 :: OSC
oscList1 = Message "/g_queryTree.reply"
                [Int 1, -- containing control parameters
                 Int 1,Int 2, -- default group, with 2 child elements
                 Int 1000,Int (-1),
                 String "simplePercSine",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",Float 0.0,
                 String "amp",Float 0.10000000149011612,
                 String "freq",Float 440.0,
                 String "out",Float 0.0,
                 Int 1001,Int (-1),
                 String "simplePercSine",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",Float 0.0,
                 String "amp",Float 0.10000000149011612,
                 String "freq",Float 440.0,
                 String "out",Float 0.0]

tree1 :: SCTree
tree1 =
  Group 0
    [Group 1
      [Synth 1000 "simplePercSine"
        ["sustain" := 0.800000011920929,
         "trig" :<- 101,
         "amp" := 0.10000000149011612,
         "freq" := 440,
         "out" := 0],
       Group 10
         [Group 100
           [Group 101
             [Synth 1011 "simplePercSine"
              ["sustain" := 0.8,
               "trig" :<- 102,
               "amp" := 0.1,
               "freq" := 330,
               "out" := 0]]]],
       Synth 1001 "simplePercSine"
         ["sustain" := 0.800000011920929,
          "trig" :<- 103,
          "amp" := 0.10000000149011612,
          "freq" := 440,
          "out" := 0]]]

oscList2 :: OSC
oscList2 =
    Message "/g_queryTree.reply"
                [Int 1, -- containing control parameters
                 Int 0,Int 1, -- root node, with 1 child element
                 Int 1,Int 5, -- default group, with 5 child element
                 Int 2,Int 0, -- group 2, no child element
                 Int 3,Int 4, -- group 3, 4 child elements.
                 Int 1002,Int (-1), -- node 1002,
                 String "param",Int 4, -- name is "param", 4 control params.
                 String "idx",String "c103", -- idx, from control bus 103.
                 String "parambuf",Float 10.0,
                 String "trig",String "c102",
                 String "out",Float 100.0,
                 Int 1003,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c103",
                 String "parambuf",Float 12.0,
                 String "trig",String "c102",
                 String "out",Float 101.0,
                 Int 1004,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c203",
                 String "parambuf",Float 20.0,
                 String "trig",String "c202",
                 String "out",Float 200.0,
                 Int 1005,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c203",
                 String "parambuf",Float 22.0,
                 String "trig",String "c202",
                 String "out",Float 201.0,
                 Int 4,Int 2,
                 Int 1000,Int (-1),
                 String "para4",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",String "c102",
                 String "amp",String "c100",
                 String "freq",String "c101",
                 String "out",Float 104.0,
                 Int 1001,Int (-1),
                 String "para4",Int 5,
                 String "sustain",Float 2.0,
                 String "trig",String "c202",
                 String "amp",String "c200",
                 String "freq",String "c201",
                 String "out",Float 204.0,
                 Int 5,Int 2,
                 Int 1006,Int (-1),
                 String "simpleReverb",Int 4,
                 String "damp",Float 0.5,
                 String "room",Float 0.8999999761581421,
                 String "mix",Float 0.5,
                 String "in",Float 104.0,
                 Int 1007,Int (-1),
                 String "simpleReverb",Int 4,
                 String "damp",Float 0.8999999761581421,
                 String "room",Float 0.5,
                 String "mix",Float 0.5,
                 String "in",Float 204.0,
                 Int 6,Int 2,
                 Int 1008,Int (-1),
                 String "simplePanGain",Int 3,
                 String "pan",Float (-0.800000011920929),
                 String "gain",Float 1.0,
                 String "bus",Float 104.0,
                 Int 1009,Int (-1),
                 String "simplePanGain",Int 3,
                 String "pan",Float 0.800000011920929,
                 String "gain",Float 1.0,
                 String "bus",Float 204.0]

oscList3 :: OSC
oscList3 =
    Message "/g_queryTree.reply"
    [Int 1,
     Int 1, Int 2,
     Int 2, Int 0,
     Int 3, Int 0]
