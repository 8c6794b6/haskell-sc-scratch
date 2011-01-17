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
    SCNode(..)
  , NodeId
  , SynthName
  , SynthParam(..)
  , ParamName
  , ParamValue
  , BusId

    -- * Parser
  , parseOSC

    -- * Converter
  , treeToOSC
  , paramToTuple
  , drawSCNode

    -- * Communicating with server
  , getTree
  , getRootNode
  , mkTree
  , printTree
  , printRootNode
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
-- Converting functions
--
------------------------------------------------------------------------------

-- | SCNode to [OSC].
--
-- OSC list contains \"g_new\", \"s_new\", and \"n_map\" messages to build
-- the given SCNode.
treeToOSC :: SCNode -> [OSC]
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

------------------------------------------------------------------------------
--
-- Communicating with server
--
------------------------------------------------------------------------------

-- | Get root node.
getRootNode :: (Transport t) => t -> IO SCNode
getRootNode = getTree 0

-- | Get node with specifying node id.
getTree :: (Transport t) => Int -> t -> IO SCNode
getTree n fd = do
  send fd (queryTree n)
  m <- wait fd "/g_queryTree.reply"
  return $ parseOSC m

-- | Send OSC message for constructing given @SCNode@.
mkTree :: (Transport t) => SCNode -> t -> IO ()
mkTree t = \fd -> do
  t0 <- utcr
  send fd (Bundle (UTCr (t0 + 0.1)) (treeToOSC t))

-- | Print current SCNode entirely.
printRootNode :: (Transport t) => t -> IO ()
printRootNode = printTree 1

-- | Prints current SCNode with specifying node id.
printTree :: Transport t => Int -> t -> IO ()
printTree n fd = getTree n fd >>= putStr . drawSCNode
