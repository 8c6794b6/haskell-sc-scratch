{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | Playing with mapping nodes to synth.
--
-- TODO:
--
-- * Rewrite @SCTree -> OSC@ function in parser style.
--

module SCTree where

import Reusable (queryTree)
import Instances

import Sound.SC3
import Sound.OpenSoundControl

import Control.Monad
import Data.List
import Data.Generics
import Data.Data
import Data.Typeable
import Data.Tree

data SCTree = Group NodeId [SCTree]
            | Synth NodeId SynthName [SynthParam]
              deriving (Eq,Read,Show,Data,Typeable)

type NodeId = Int
type SynthName = String

data SynthParam = ParamName := ParamValue
                | ParamName :<- BusId
                  deriving (Eq,Show,Read,Data,Typeable)

type ParamName = String
type ParamValue = Double
type BusId = Int

infixr 5 :=
infixr 5 :<-

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
         [] -> []
         (c:cs) -> [(c,cs)]

int :: DatumParser Int
int = do {d <- datum; case d of {Int x -> return x; _ -> mzero}}

double :: DatumParser Double
double = do {d <- datum; case d of {Double x -> return x; _ -> mzero}}

float :: DatumParser Double
float = do {d <- datum; case d of {Float x -> return x; _ -> mzero}}

string :: DatumParser String
string = do {d <- datum; case d of {String x -> return x; _ -> mzero}}

-- | Parse osc message returned from "/g_queryTree" and returns haskell
-- representation of scsynth node tree.
-- Only working with osc message including synth control parameters.
parseOSC :: OSC -> SCTree
parseOSC (Message s ds)
    | s == "/g_queryTree.reply" = parseDatum ds
    | otherwise = error "not a /g_queryTree.reply message"

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
    Float x -> return $ name := x
    Double x -> return $ name := x
    String x -> return $ name :<- (read $ tail x)

toGroupTree :: SCTree -> SCTree
toGroupTree (Group gid ts) = Group gid (map toGroupTree ts')
    where ts' = filter isGroup ts
          isGroup (Group _ _) = True
          isGroup (Synth _ _ _) = False
toGroupTree (Synth _ _ _) = error "toGroupTree: Root is Synth."

-- | SCTree to [OSC] with g_new, s_new and n_map.
treeToOSC :: SCTree -> [OSC]
treeToOSC t = treeToNew t ++ nMap t

-- | SCTree to [OSC] with g_new and s_new.
-- Using @everything@ from syb.
treeToNew :: SCTree -> [OSC]
treeToNew t = everything (++) ([] `mkQ` f) t
    where
      f (Group nId ts) = concatMap (g nId) ts
      f _ = []
      g gId (Synth nId name params)
          = [s_new name nId AddToTail gId (concatMap paramToTuple params)]
      g gId (Group gId' _)  = [g_new [(gId',AddToTail,gId)]]

-- | Extract "/g_new" messages.
gNew :: SCTree -> OSC
gNew t = squash $ gNew' t' []
    where squash m = Message "/g_new" (everything (++) ([] `mkQ` f) m)
          f (Int i) = [Int i]
          f _ = []
          t' = toGroupTree t

gNew' :: SCTree -> [OSC] -> [OSC]
gNew' (Group gId ts) msg = addToParent gId ts msg
gNew' _ msg = msg

addToParent :: NodeId -> [SCTree] -> [OSC] -> [OSC]
addToParent gId ((Group gId' ts'):ts) msg
    = g_new [(gId',AddToTail,gId)] : addToParent gId' ts' [] ++
      addToParent gId ts msg
addToParent gId ((Synth nId name ps):ts) msg
    = [s_new name nId  AddToTail gId (concatMap paramToTuple ps)] ++
      addToParent gId ts msg
addToParent gId [] msg = msg

paramToTuple :: SynthParam -> [(String,Double)]
paramToTuple (name := val) = [(name,val)]
paramToTuple _ = []

paramToMap :: NodeId -> SynthParam -> [OSC]
paramToMap i (n :<- b) = [n_map i [(n,b)]]
paramToMap _ _ = []

-- -- | Extract "/s_new" messages.
-- sNew :: SCTree -> OSC
-- sNew = undefined

toSynthList :: SCTree -> [(NodeId,SCTree)]
toSynthList = everything (++) ([] `mkQ` f)
    where
      f (Group gId ts) = concatMap (g gId) ts
      f _ = []
      g gId s@(Synth _ _ _) = [(gId,s)]
      g gid (Group _ _) = []

-- | Extract "/n_map" messages.
nMap :: SCTree -> [OSC]
nMap = everything (++) ([] `mkQ` f)
    where
      f (Group _ ts) = concatMap g ts
      f _ = []
      g (Synth nId _ ps) = concatMap (paramToMap nId) ps
      g _ = []

-- | Get current node mapping representation of @SCTree@.
getTree :: (Transport t) => t -> IO SCTree
getTree fd = queryTree fd >>= return . parseOSC

-- | Send node mapping OSC message scsynth, defined by @SCTree@.
mkTree :: (Transport t) => SCTree -> t -> IO ()
mkTree t = \fd -> do
             t0 <- utcr
             send fd (Bundle (UTCr (t0 + 0.1)) (treeToOSC t))
-- mkTree tree = \fd ->
--              send fd (Bundle (NTPi 0) (gNew' tree [] ++ nMap tree))


--
-- For converting SCTree to Tree datatype in Data.Tree.Tree.
-- Data.Tree.Tree is a rose tree, but SCTree datatype is not.
--

type SCTree' = Tree SCNode

data SCNode = G NodeId
            | S NodeId SynthName [SynthParam]
              deriving (Eq)

instance Show SCNode where
    show (G nid) = "Group " ++ show nid
    show (S nid name ps) = "Synth " ++ show nid ++ " " ++ name ++ " " ++ show ps

toRose :: SCTree -> SCTree'
toRose (Group nid ns) = Node (G nid) (map toRose ns)
toRose (Synth nid name ps) = Node (S nid name ps) []

-- fromRose :: SCTree' -> SCTree
-- fromRose = undefined

drawSCTree :: SCTree -> String
drawSCTree = drawTree . fmap show . toRose

-- | Prints current SCTree.
printTree :: Transport t => t -> IO ()
printTree fd = getTree fd >>= putStr . drawSCTree

queryAllNodes :: Transport t => t -> IO ()
queryAllNodes = \fd -> getTree fd >>= putStr . drawSCTree


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

scTree1 :: SCTree
scTree1
    = Group 0
      [Group 1
       [Synth 1000 "simplePercSine"
        ["sustain" := 0.800000011920929,
         "trig" :<- 1,
         "amp" := 0.10000000149011612,
         "freq" := 440,
         "out" := 0],
        Group 10
        [Group 100
         [Group 101
          [Synth 1011 "simplePercSine"
           ["sustain" := 0.8,
            "trig" :<- 2,
            "amp" := 0.1,
            "freq" := 330,
            "out" := 0]]]],

        Synth 1001 "simplePercSine"
        ["sustain" := 0.800000011920929,
         "trig" :<- 3,
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

-- | Testing syb.
getAllNames :: Data a => a -> [SynthName]
getAllNames = everything (++) ([] `mkQ` f)
    where
      f (Synth _ n _) = [n]
      f a = []

-- | Another test for syb.
getAllStrings :: Data a => a -> [String]
getAllStrings  = everything (++) ([] `mkQ` f)
    where
      f (String s) = [s]
      f _ = []