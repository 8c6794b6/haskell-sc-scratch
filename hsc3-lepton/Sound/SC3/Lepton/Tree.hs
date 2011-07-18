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
  , parseNode

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
  , treeToNewWith
  , treeToSet
  , paramToTuple
  , drawSCNode
  , renderNode
  , prettyDump

    -- * Util
  , paramName
  , updateParams
  )  where

import Control.Monad
import Data.Function (on)
import Data.Generics (Data, Typeable)
import Data.List (unionBy)
import Text.PrettyPrint hiding (int, double)

import Sound.SC3
import Sound.OpenSoundControl

import Sound.SC3.Lepton.Instance ()
import Sound.SC3.Lepton.Parser
import Sound.SC3.Lepton.Util (queryTree, n_mapa)

import qualified Text.PrettyPrint as P

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
                | ParamName :<= BusId     -- ^ Mapped audio bus id
                  deriving (Eq,Read,Data,Typeable)

instance Show SynthParam where
  show (f:=x)  = show f ++ ":=" ++ show x
  show (f:<-x) = show f ++ ":<-" ++ show x
  show (f:<=x) = show f ++ ":<=" ++ show x

type ParamName = String
type ParamValue = Double
type BusId = Int

infixr 5 :=
infixr 5 :<-
infixr 5 :<=

-- | Parse osc message returned from \"/g_queryTree\" and returns haskell
-- representation of scsynth node tree.
-- Only working with osc message including synth control parameters.
parseNode :: OSC -> SCNode
parseNode o = case o of
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
    else Group nId `fmap` replicateM numChild parseGroup

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
    Float x   -> return $ name := x
    Double x  -> return $ name := x
    String xs -> case xs of
      'c':rest -> return $ name :<- read rest
      'a':rest -> return $ name :<= read rest
      _        -> error $ "Unknown param: " ++ xs
    Int x     -> return $ name := fromIntegral x
    e         -> error $ "Cannot make param from: " ++ show e

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
  return $ parseNode m

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
printNode n fd = getNode n fd >>= putStrLn . renderNode True

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
treeToNew = treeToNewWith AddToTail

treeToNewWith :: AddAction -- ^ Add action for this node
              -> NodeId    -- ^ Target node id
              -> SCNode    -- ^ New node to add
              -> [OSC]
treeToNewWith aa tId tree = f tId tree
  where
    f i t = case t of
      Group j ns   -> g_new [(j,aa,i)]:concatMap (f j) ns
      Synth j n ps -> s_new n j aa i (concatMap paramToTuple ps):g j ps
    g i xs | not (null cs) && not (null as) = n_map i cs:n_mapa i as:[]
           | not (null cs) && null as       = [n_map i cs]
           | null cs && not (null as)       = [n_mapa i as]
           | otherwise                      = []
      where (cs,as) = foldr h ([],[]) xs
    h x (cs,as) = case x of
      (name:<-bus) -> ((name,bus):cs,as)
      (name:<=bus) -> (cs,(name,bus):as)
      _            -> (cs,as)


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
    g k ps | not (null cs) && not (null as) = n_map k cs:n_mapa k as:[]
           | not (null cs) && null as       = [n_map k cs]
           | null cs && not (null as)       = [n_mapa k as]
           | otherwise                      = []
      where (cs,as) = foldr h ([],[]) ps
    h x (cs,as) = case x of
      (name:<-bus) -> ((name,bus):cs,as)
      (name:<=bus) -> (cs,(name,bus):as)
      _            -> (cs,as)


------------------------------------------------------------------------------
--
-- Utils
--
------------------------------------------------------------------------------

paramToTuple :: SynthParam -> [(String,Double)]
paramToTuple (name := val) = [(name,val)]
paramToTuple _ = []

paramName :: SynthParam -> ParamName
paramName x = case x of
  (n := _)  -> n
  (n :<- _) -> n
  (n :<= _) -> n

updateParams :: [SynthParam] -> SCNode -> SCNode
updateParams ps node = case node of
  Synth i n ps' -> Synth i n (unionBy ((==) `on` paramName) ps ps')
  g             -> g

------------------------------------------------------------------------------
--
-- Converting functions
--
------------------------------------------------------------------------------

-- | Pretty prints SCNode.
renderNode :: Bool -> SCNode -> String
renderNode detail = render . n2doc where
  n2doc n = case n of
    Group i ns   -> P.int i <+> text "group" $$ vcat (map (nest 3 . n2doc) ns)
    Synth i name ps ->
      P.int i <+> text name $$
      (if detail then hsep (map (nest 2 . p2doc) ps) else empty)
  p2doc p = case p of
    n:=v  -> text n <> char ':' <+> P.double v
    n:<-v -> text n <> char ':' <+> char 'c' <> P.int v
    n:<=v -> text n <> char ':' <+> char 'a' <> P.int v

-- | Dump SCNode. Dumped string could be parsed to read function.
prettyDump :: SCNode -> String
prettyDump = render . n2doc where
  n2doc :: SCNode -> Doc
  n2doc n = case n of
    Group i ns ->
      text "Group" <+> signedInt i <+> char '[' $$
      (nest 2 $ vcat ((punctuate comma (map n2doc ns)) ++ [text "]"]))
    Synth i name ps ->
      text "Synth" <+> signedInt i <+> doubleQuotes (text name) <+> char '[' $$
      (nest 2 $ vcat ((punctuate comma (map p2doc ps)) ++ [text "]"]))
  p2doc :: SynthParam -> Doc
  p2doc p = text $ show p

signedInt :: Int -> Doc
signedInt n | n < 0     = char '(' <> P.int n <> char ')'
            | otherwise = P.int n

-- | Draw SCNode data.
drawSCNode :: SCNode -> String
drawSCNode = renderNode True
