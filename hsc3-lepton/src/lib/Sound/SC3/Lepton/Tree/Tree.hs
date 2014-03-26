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
module Sound.SC3.Lepton.Tree.Tree
  ( -- * Types
    SCNode(..)
  , NodeId
  , nodeId
  , SynthName
  , SynthParam(..)
  , ParamName
  , ParamValue
  , BusId

    -- * Parser
  , parseNode

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
  , nodeIds
  , hasUniqueIds
  )  where

import Control.Monad
import Data.ByteString.Char8 (unpack)
import Data.Function (on)
import Data.Data
import Data.List (unionBy)
import Text.PrettyPrint hiding (int, double)

import Data.Generics.Uniplate.Data
import Sound.SC3
import Sound.OSC hiding (int32, string)

import Sound.SC3.Lepton.Instance ()
import Sound.SC3.Lepton.Parser.Datum

import qualified Text.PrettyPrint as P
import qualified Data.IntSet as IS


------------------------------------------------------------------------------
--
-- Types
--
------------------------------------------------------------------------------

-- | Data type for representing Group and Synth node in scsynth.
data SCNode = Group NodeId [SCNode]                -- ^ Group node
            | Synth NodeId SynthName [SynthParam]  -- ^ Synth node
              deriving (Eq,Read,Show,Data,Typeable)

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

nodeId :: SCNode -> Int
nodeId (Group i _) = i
nodeId (Synth i _ _) = i

-- | Parse osc message returned from \"/g_queryTree\" and returns haskell
-- representation of scsynth node tree.
-- Only working with osc message including synth control parameters.
parseNode :: Message -> SCNode
parseNode o = case o of
  Message "/g_queryTree.reply" ds -> case parseDatum parseGroup (tail ds) of
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
  nId <- int32
  numChild <- int32
  if numChild < 0
    then parseSynth (fromIntegral nId)
    else Group (fromIntegral nId) `fmap` replicateM (fromIntegral numChild) parseGroup

parseSynth :: Int -> DatumParser SCNode
parseSynth nId = do
  name <- unpack `fmap` string
  numParams <- int32
  params <- replicateM (fromIntegral numParams) parseParam
  return $ Synth nId name params

-- | Parse parameter values for each synth.
--
-- Audio bus numbers shown for mapped audio controls are correct only when the
-- number of audio busses used in the server equals to 128, the default value.
--
parseParam :: DatumParser SynthParam
parseParam = do
  name <- unpack `fmap` string
  val <- datum
  case val of
    Float x   -> return $ name := realToFrac x
    Double x  -> return $ name := x
    ASCII_String xs ->
        let xs' = unpack xs
        in  case xs' of
            'c':rest -> do
                let busNum = read rest :: Int
                return $ if busNum > 0
                         then name :<- busNum
                         else name :<= ((busNum - 4) `div` 64) + 129
            'a':rest -> return $ name :<= read rest
            _        -> error $ "Unknown param: " ++ xs'
    Int32 x     -> return $ name := fromIntegral x
    e         -> error $ "Cannot make param from: " ++ show e

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
          -> [Message]
treeToNew = treeToNewWith AddToTail

treeToNewWith :: AddAction -- ^ Add action for this node
              -> NodeId    -- ^ Target node id
              -> SCNode    -- ^ New node to add
              -> [Message]
treeToNewWith aa tId tree = f tId tree
  where
    f i t = case t of
      Group j ns   -> g_new [(j,aa,i)]:concatMap (f j) ns
      Synth j n ps -> s_new n j aa i (concatMap paramToTuple ps):g j ps
    g i xs | not (null cs) && not (null as) = [n_map i cs,n_mapa i as]
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
          -> [Message]
treeToSet tree = f tree
  where
    f t = case t of
      Group _ ns   -> concatMap f ns
      Synth _ _ [] -> []
      Synth j _ ps -> n_set j (concatMap paramToTuple ps):g j ps
    g k ps | not (null cs) && not (null as) = [n_map k cs,n_mapa k as]
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

hasUniqueIds :: SCNode -> Bool
hasUniqueIds n = listSize == setSize where
  setSize = IS.size . IS.fromList $ l
  listSize = length l
  l = nodeIds n

nodeIds :: SCNode -> [Int]
nodeIds n = [f n'|n' <- universe n, let f (Synth j _ _) = j; f (Group j _) = j]

------------------------------------------------------------------------------
--
-- Pretty printers
--
------------------------------------------------------------------------------

-- | Draw SCNode data.
drawSCNode :: SCNode -> String
drawSCNode = renderNode True

-- | Pretty prints SCNode in same format as '/g_dumpTree' OSC message.
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

-- | Dump SCNode. Dumped string could be parsed with 'read' function.
prettyDump :: SCNode -> String
prettyDump = render . n2doc where
  n2doc :: SCNode -> Doc
  n2doc n = case n of
    Group i ns ->
      text "Group" <+> signedInt i $$
      nest 2 (brackets $ vcat (punctuate comma (map n2doc ns)))
    Synth i name ps ->
      text "Synth" <+> signedInt i <+> doubleQuotes (text name) $$
      nest 2 (brackets $ vcat (punctuate comma (map p2doc ps)))
  p2doc p = case p of
    (k:=v)  -> doubleQuotes (text k) <> text ":=" <> signedDouble v
    (k:<-v) -> doubleQuotes (text k) <> text ":<-" <> signedInt v
    (k:<=v) -> doubleQuotes (text k) <> text ":<=" <> signedInt v

-- | Show signed int.
signedInt :: Int -> Doc
signedInt n | n < 0     = parens (P.int n)
            | otherwise = P.int n

-- | Show signed double.
signedDouble :: Double -> Doc
signedDouble n | n < 0     = parens (P.double n)
               | otherwise = P.double n
