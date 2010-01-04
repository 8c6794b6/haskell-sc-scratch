{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | Playing with mapping nodes to synth.
--
-- TODO:
-- * Remove @(++)@ from parseOSC, or use parsec.
-- * Remove @nub@ from @nMap@.
-- * Write pretty printer for SCTree.
--

module SimpleMapping where

import SimpleUGens
import SimpleNotes

import Reusable (queryTree)
import Instances

import Sound.SC3
import Sound.OpenSoundControl

import Data.List
import Data.Generics
import Data.Data
import Data.Typeable

-- import Text.ParserCombinators.Parsec

data SCTree = Group NodeId [SCTree]
            | Synth NodeId SynthName [SynthParam]
              deriving (Eq,Read,Show,Data,Typeable)

type NodeId = Int
type SynthName = String

data SynthParam = ParamName := ParamValue
               deriving (Eq,Read,Show,Data,Typeable)

type ParamName = String

data ParamValue = PVal Double
                | PBus Int
                  deriving (Eq,Read,Show,Data,Typeable)

infixr 5 :=

-- | Parse osc message returned from "/g_queryTree" and returns haskell
-- representation of the node tree.
-- Only working with osc message including synth control parameters.
parseOSC :: OSC -> SCTree
parseOSC (Message s ds)
    | s == "/g_queryTree.reply" = fst (parseDatum (tail ds))
    | otherwise = error "not a /g_queryTree.reply message"
parseOSC _ = error "OSC mssage is not a 'Message', might be 'Bundle'"

parseDatum :: [Datum] -> (SCTree,[Datum])
parseDatum [] = error "empty list passed to parseDatum"
parseDatum (Int x:Int y:ds)
    | y < 0 = parseSynth x ds
    | otherwise = (Group x ts,ds')
    where (ts,ds') = parseDatumFor y ds []

parseDatumFor :: Int -> [Datum] -> [SCTree] -> ([SCTree],[Datum])
parseDatumFor left ds ts
    | left == 0 = (ts,ds)
    | otherwise = parseDatumFor (left-1) ds' (ts++[t])
    where (t,ds') = parseDatum ds

parseSynth :: NodeId -> [Datum] -> (SCTree,[Datum])
parseSynth nid (String name:Int num:ds) = (Synth nid name params,ds')
    where (params,ds') = parseParamsFor num ds []

parseParamsFor :: Int -> [Datum] -> [SynthParam] -> ([SynthParam],[Datum])
parseParamsFor 0 ds ps = (ps,ds)
parseParamsFor n ds ps = parseParamsFor (n-1) ds' (ps++[ps'])
    where (ps',ds') = parseParam ds

parseParam :: [Datum] -> (SynthParam,[Datum])
parseParam (String n:d:ds) = (n := d',ds)
    where
      d' = case d of
             Float x -> PVal x
             String s -> PBus (read (tail s))

toGroupTree :: SCTree -> SCTree
toGroupTree (Group gid ts) = Group gid (map toGroupTree ts')
    where ts' = filter isGroup ts
          isGroup (Group _ _) = True
          isGroup (Synth _ _ _) = False
toGroupTree (Synth _ _ _) = undefined

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
    = g_new [(gId',AddToTail,gId)] : addToParent gId' ts' [] ++ addToParent gId ts msg
addToParent gId ((Synth nId name ps):ts) msg
    = [s_new name nId  AddToTail gId (concatMap paramToTuple ps)] ++
      addToParent gId ts msg
addToParent gId [] msg = msg

paramToTuple :: SynthParam -> [(String,Double)]
paramToTuple (name := val)
    = case val of
        PVal d -> [(name,d)]
        _      -> []

paramToMap :: NodeId -> SynthParam -> [OSC]
paramToMap _ (_ := (PVal v)) = []
paramToMap i (n := (PBus b)) = [n_map i [(n,b)]]

-- | Extract "/s_new" messages.
sNew :: SCTree -> OSC
sNew = undefined

toSynthList :: SCTree -> [(NodeId,SCTree)]
toSynthList = nub . everything (++) ([] `mkQ` f)
    where
      f (Group gId ts) = g gId ts
      f _ = []
      g gId ((Group gId' ts'):ts) = g gId' ts' ++ g gId ts
      g gId (s@(Synth _ _ _):ts) = (gId,s):g gId ts
      g gId [] = []

-- | Extract "/n_map" messages.
--
-- XXX: Remove @nub@ function!
nMap :: SCTree -> [OSC]
nMap = nub . everything (++) ([] `mkQ` f)
    where
      f (Group _ ts) = concatMap f ts
      f (Synth nId _ ps) = concatMap (paramToMap nId) ps

-- | Get current node mapping representation of @SCTree@.
getTree :: (Transport t) => t -> IO SCTree
getTree fd = queryTree fd >>= return . parseOSC

-- | Send node mapping OSC message scsynth, defined by @SCTree@.
mkTree :: (Transport t) => SCTree -> t -> IO ()
mkTree tree = \fd ->
             send fd (Bundle (NTPi 0) (gNew' tree [] ++ nMap tree))

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
scTree1 = Group 1
               [Synth 1000 "simplePercSine"
                ["sustain" := (PVal 0.800000011920929),
                 "trig" := (PVal 0),
                 "amp" := (PVal 0.10000000149011612),
                 "freq" := (PVal 440),
                 "out" := (PVal 0)],
                Synth 1001 "simplePercSine"
                ["sustain" := (PVal 0.800000011920929),
                 "trig" := (PVal 0),
                 "amp" := (PVal 0.10000000149011612),
                 "freq" := (PVal 440),
                 "out" := (PVal 0)]]

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