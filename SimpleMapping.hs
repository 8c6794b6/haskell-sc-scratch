{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | Playing with mapping nodes to synth.
--

module SimpleMapping where

import SimpleUGens
import SimpleNotes

import Reusable
import Instances

import Sound.SC3
import Sound.OpenSoundControl

import Data.Generics
import Data.Data
import Data.Typeable

-- import Text.ParserCombinators.Parsec

data SCTree = Group NodeId [SCTree]
            | Synth NodeId SynthName [SynthParam]
              deriving (Eq,Show,Data,Typeable)

type NodeId = Int
type SynthName = String

data SynthParam = SynthParam ParamName ParamValue
               deriving (Eq,Show,Data,Typeable)

type ParamName = String

data ParamValue = CValue Double
                | BusId Int
                  deriving (Eq,Show,Data,Typeable)

-- | After "withSC3 reset".
defaultGroup :: SCTree
defaultGroup = Group 0 [Group 1 []]

-- | Only working with osc message including synth control parameters.
parseSCTree :: OSC -> SCTree
parseSCTree (Message s ds)
    | s == "/g_queryTree.reply" = fst (parseSCTree' (tail ds))
    | otherwise = error "not a /g_queryTree.reply message"
parseSCTree _ = error "OSC mssage is not a 'Message', might be 'Bundle'"

parseSCTree' :: [Datum] -> (SCTree,[Datum])
parseSCTree' [] = error "empty list passed to \"parseSCTree'\""
parseSCTree' (Int x:Int y:ds)
    | y < 0 = parseSynth x ds
    | otherwise = (Group x tree,ds')
    where (tree,ds') = parseSCTreeFor y ds []

parseSCTreeFor :: Int -> [Datum] -> [SCTree] -> ([SCTree],[Datum])
parseSCTreeFor left ds ts
    | left == 0 = (ts,ds)
    | otherwise = parseSCTreeFor (left-1) ds' (ts++[t])
    where (t,ds') = parseSCTree' ds

parseSynth :: NodeId -> [Datum] -> (SCTree,[Datum])
parseSynth nid (String name:Int num:ds) = (Synth nid name params,ds')
    where (params,ds') = parseParamsFor num [] ds

parseParamsFor :: Int -> [SynthParam] -> [Datum] -> ([SynthParam],[Datum])
parseParamsFor 0 ps ds = (ps,ds)
parseParamsFor n ps ds = parseParamsFor (n-1) (ps++[ps']) ds'
    where (ps',ds') = parseParam ds

parseParam :: [Datum] -> (SynthParam,[Datum])
parseParam (String n:d:ds) = (SynthParam n d',ds)
    where
      d' = case d of
             Float x -> CValue x
             String s -> BusId (read (tail s))

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
                [SynthParam "sustain" (CValue 0.8),
                 SynthParam "trig" (CValue 0),
                 SynthParam "amp" (CValue 0.1),
                 SynthParam "freq" (CValue 440),
                 SynthParam "out" (CValue 0)],
                Synth 1000 "simplePercSine"
                [SynthParam "sustain" (CValue 0.8),
                 SynthParam "trig" (CValue 0),
                 SynthParam "amp" (CValue 0.1),
                 SynthParam "freq" (CValue 440),
                 SynthParam "out" (CValue 0)]]

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