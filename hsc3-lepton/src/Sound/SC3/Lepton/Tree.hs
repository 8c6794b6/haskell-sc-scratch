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
  ( -- * Usage Examples

    -- ** Quick interaction
    -- $example_interactive

    -- ** Routing nodes
    -- $example_declarative

    -- * Types
    SCNode(..)
  , NodeId
  , SynthName
  , SynthParam(..)
  , ParamName
  , ParamValue
  , BusId

    -- * Parser
  , parseOSC

    -- * Communicating with server
  , addNode
  , getNode
  , setNode
  , delNode
  , modifyNode
  , printNode
  , getRootNode
  , printRootNode

    -- * Converter
  , treeToNew
  , treeToSet
  , paramToTuple
  , drawSCNode
  )  where

import Control.Monad
import Data.Generics (Data, Typeable)
import Data.Tree
import Text.Printf (printf)

import Sound.SC3
import Sound.OpenSoundControl

import Sound.SC3.Lepton.Instance ()
import Sound.SC3.Lepton.Parser
import Sound.SC3.Lepton.Util (queryTree)

-- $example_interactive
--
-- Dump the contents of running synth nodes from ghci:
--
-- > > :m + Sound.SC3
-- > > withSC3 reset
-- > > withSC3 printRootNode
-- > Group 0
-- > `-Group 1
--
-- Dump again after adding synth nodes:
--
-- > > audition $ out 0 $ sinOsc ar (control kr "freq" 440) 0 * 0.3
-- > > audition $ out 0 $ sinOsc ar (control kr "freq" 330) 0 * 0.3
-- > > withSC3 printRootNode
-- > Group 0
-- > `-Group 1
-- >   +-Synth -48 Anonymous
-- >   | `-[freq := 440.00]
-- >   `-Synth -56 Anonymous
-- >     `-[freq := 330.00]
--
-- Updating synth nodes with using generic transforming function, e.g.
-- @transform@ from uniplate package:
--
-- > > :m + Data.Generics.Uniplate.Data
-- > > t <- withSC3 getRootNode
-- > > let f (Synth i n ps) = Synth (2000 + abs i) "default" ps; f x = x
-- > > withSC3 $ addNode 0 $ transform f t
--
-- Using @transformBi@:
--
-- > > let g ("freq":=f) = "freq":=(f*2); g x = x
-- > > withSC3 reset
-- > > withSC3 $ addNode 0 $ transformBi g t
--

-- $example_declarative
--
-- Write node structure and send it to scsynth. \"fmod\" parameters
-- in synth \"bar\" are mapped from control rate outputs of synth \"foo\".
--
-- > import Sound.OpenSoundControl
-- > import Sound.SC3
-- > import Sound.SC3.Lepton
-- >
-- > main :: IO ()
-- > main = withSC3 playFooBar
-- >
-- > playFooBar :: (Transport t) => t -> IO ()
-- > playFooBar fd = do
-- >   mapM (\(n,u) -> async fd . d_recv $ synthdef n u) [("foo",foo),("bar",bar)]
-- >   addNode 0 nodes fd
-- >
-- > nodes :: SCNode
-- > nodes =
-- >   Group 1
-- >     [Group 10
-- >       [Synth 1000 "foo"
-- >          ["out":=100,"amp":=100,"freq":=1.66]
-- >       ,Synth 1001 "foo"
-- >          ["out":=101,"amp":=80,"freq":=3.33]]
-- >     ,Group 11
-- >       [Synth 1100 "bar"
-- >          ["amp":=0.1,"pan":=0.5,"freq":=110,"fmod":<-100]
-- >       ,Synth 1101 "bar"
-- >          ["amp":=0.1,"pan":=(-0.5),"freq":=330,"fmod":<-101]]]
-- >
-- > foo :: UGen
-- > foo = out outBus (sinOsc kr freq 0 * amp)
-- >
-- > bar :: UGen
-- > bar = out 0 $ pan2 (saw ar (fmod + freq) * amp) pan 1
-- >
-- > outBus, amp, freq, pan, fmod :: UGen
-- > outBus = control kr "out" 0
-- > amp = control kr "amp" 0.3
-- > freq = control kr "freq" 440
-- > pan = control kr "pan" 0
-- > fmod = control kr "fmod" 0

------------------------------------------------------------------------------
--
-- Types
--
------------------------------------------------------------------------------

-- | Data type for representing Group and Synth node in scsynth.
data SCNode = Group NodeId [SCNode]                -- ^ Group node
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

-- | Parse osc message returned from \"/g_queryTree\" and returns haskell
-- representation of scsynth node tree.
-- Only working with osc message including synth control parameters.
parseOSC :: OSC -> SCNode
parseOSC o = case o of
  Message "/g_queryTree.reply" ds -> case parse parseGroup (tail ds) of
    Right tree -> tree
    Left err   -> error $ show err
  _                               -> error "not a /g_queryTree.reply response"

--
-- With using simple parser without parsec
--
-- parseOSC :: OSC -> SCNode
-- parseOSC o = case o of
--   Message "/g_queryTree.reply" ds -> fst $ head $ parse parseGroup (tail ds)
--   _                               -> error "not a g_queryTree.reply message"

parseGroup :: DatumParser SCNode
parseGroup = do
  nId <- int
  numChild <- int
  if numChild < 0
    then parseSynth nId
    else do
      ts <- replicateM numChild parseGroup
      return $ Group nId ts

parseSynth :: Int -> DatumParser SCNode
parseSynth nId = do
  name <- string
  numParams <- int
  params <- replicateM numParams parseParam
  return $ Synth nId name params

parseParam :: DatumParser SynthParam
parseParam = do
  name <- string
  val <- datum
  case val of
    Float x  -> return $ name := x
    Double x -> return $ name := x
    String x -> return $ name :<- (read $ tail x)
    Int x    -> return $ name := fromIntegral x
    e        -> error $ "Cannot make param from: " ++ show e

------------------------------------------------------------------------------
--
-- Communicating with server
--
------------------------------------------------------------------------------

-- | Send OSC message for constructing given @SCNode@.
-- New node will be added to tail of target id.
addNode :: (Transport t)
        => NodeId -- ^ Traget node id to add
        -> SCNode -- ^ New node
        -> t      -- ^ Scsynth connection
        -> IO ()
addNode tId tree = \fd -> do
  t0 <- utcr
  send fd (Bundle (UTCr (t0 + 0.1)) (treeToNew tId tree))

-- | Get node with specifying node id.
getNode :: (Transport t)
        => NodeId     -- ^ Node id to get
        -> t          -- ^ Connection
        -> IO SCNode
getNode n fd = do
  send fd (queryTree n)
  m <- wait fd "/g_queryTree.reply"
  return $ parseOSC m

-- | Send OSC message for setting given @SCNode@.
setNode :: (Transport t)
        => SCNode -- ^ Node with new parameters
        -> t
        -> IO ()
setNode tree = \fd -> do
  send fd $ Bundle immediately (treeToSet tree)

-- | Free the nodes specified with given node ids.
delNode :: (Transport t)
        => [NodeId]      -- ^ Node ids to free
        -> t
        -> IO ()
delNode ns = \fd -> send fd $ n_free ns

-- | Experimental.
--
-- Modify running node with given function.
--
-- Implemented with freeing all nodes and adding transformed new nodes.
--
modifyNode :: (Transport t)
           => (SCNode -> SCNode) -- ^ Function to apply
           -> t
           -> IO ()
modifyNode f = \fd -> do
  t <- getRootNode fd
  reset fd
  send fd $ Bundle immediately (treeToNew 0 (f t))

-- | Prints current SCNode with specifying node id.
printNode :: Transport t => Int -> t -> IO ()
printNode n fd = getNode n fd >>= putStr . drawSCNode

--
-- Variants for root node
--

-- | Get root node.
getRootNode :: (Transport t) => t -> IO SCNode
getRootNode = getNode 0

-- | Print current SCNode entirely.
printRootNode :: (Transport t) => t -> IO ()
printRootNode = printNode 0

------------------------------------------------------------------------------
--
-- Converting functions
--
------------------------------------------------------------------------------

-- | SCNode to [OSC] for creating new nodes.
--
-- OSC list contains \"g_new\", \"s_new\", and \"n_map\" messages to build
-- the given SCNode. New node will be added to tail of target id.
--
treeToNew :: NodeId  -- ^ Target node id
          -> SCNode  -- ^ New nodes
          -> [OSC]
treeToNew tId tree = f tId tree
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

-- | SCNode to [OSC] for updating nodes.
--
-- OSC list contains \"n_set\" and \"n_map\" messages to set parameters.
--
treeToSet :: SCNode -- ^ Node with new parameters for already exisitng nodes.
          -> [OSC]
treeToSet tree = f tree
  where
    f t = case t of
      Group _ ns   -> concatMap f ns
      Synth _ _ [] -> []
      Synth j _ ps -> n_set j (concatMap paramToTuple ps):g j ps
    g k ps | not $ null qs = [n_map k qs]
           | otherwise     = []
      where qs = foldr h [] ps
    h a b = case a of
      (name:<-bus) -> (name,bus):b
      _            -> b

paramToTuple :: SynthParam -> [(String,Double)]
paramToTuple (name := val) = [(name,val)]
paramToTuple _ = []


-- | For converting SCNode to Tree datatype in Data.Tree.Tree.
-- Data.Tree.Tree is a rose tree, but SCNode datatype is not.
type SCNode' = Tree SCN

-- | Wrapper for SCNode.
data SCN = G NodeId
         | S NodeId SynthName
         | P [SynthParam]
         deriving (Eq)

instance Show SCN where
    show (G nid) = "Group " ++ show nid
    show (S nid name) = "Synth " ++ show nid ++ " " ++ name
    show (P ps) = show ps

-- | Draw SCNode data.
drawSCNode :: SCNode -> String
drawSCNode = unlines . draw . fmap show . toRose

toRose :: SCNode -> SCNode'
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
