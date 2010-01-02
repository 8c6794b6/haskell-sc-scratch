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

import Text.ParserCombinators.Parsec

data SCTree = SynthGroup GroupId [SCTree]
            | SynthNode NodeId SynthName [NodeParam]
              deriving (Eq,Show,Data,Typeable)

type GroupId = Int
type NodeId = Int
type SynthName = String

data NodeParam = NodeParam ParamName ParamValue
               deriving (Eq,Show,Data,Typeable)

type ParamName = String

data ParamValue = CValue Double
                | BusId Int
                  deriving (Eq,Show,Data,Typeable)

defaultGroup :: SCTree
defaultGroup = SynthGroup 1 []

-- | Sample message returned from scsynth server, without group other
-- than default. 
oscList1 :: OSC
oscList1 = Message "/g_queryTree.reply" 
                [Int 1,Int 1,Int 2,
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
scTree1 = SynthGroup 1 
               [SynthNode 1000 "simplePercSine" 
                [NodeParam "sustain" (CValue 0.8),
                 NodeParam "trig" (CValue 0),
                 NodeParam "amp" (CValue 0.1),
                 NodeParam "freq" (CValue 440),
                 NodeParam "out" (CValue 0)],
                SynthNode 1000 "simplePercSine" 
                [NodeParam "sustain" (CValue 0.8),
                 NodeParam "trig" (CValue 0),
                 NodeParam "amp" (CValue 0.1),
                 NodeParam "freq" (CValue 440),
                 NodeParam "out" (CValue 0)]]

-- oscList2 = 
    -- Message "/g_queryTree.reply" 
    --             [Int 1,Int 0,Int 1, -- first "1" means this message contains
    --                                 -- control params, next "0" is the node id
    --                                 -- of this node (it means root node), and
    --                                 -- the third "1" is number of node in this
    --                                 -- tree, only the default group.

    --              Int 1,Int 5, -- node id "1", number of node in this group, 5
    --                           -- elements are in this group.

    --              Int 2,Int 0, -- node id "2", no elements are in this group.

    --              Int 3,Int 4, -- node id "3", 4 elements are in this group.

    --              Int 1002,Int (-1), -- node id "1002", "-1" indicates that this
    --                                 -- element is a synth node, not a group.

    --              String "param",Int 4, -- name of this synth, number of control
    --                                    -- parameters.

    --              String "idx",String "c103", -- pair of control name and value.
    --              String "parambuf",Float 10.0,
    --              String "trig",String "c102",
    --              String "out",Float 100.0,
                        
    --              Int 1003,Int (-1), -- 
    --              String "param",Int 4,
    --              String "idx",String "c103",
    --              String "parambuf",Float 12.0,
    --              String "trig",String "c102",
    --              String "out",Float 101.0,
    --              Int 1004,Int (-1),
    --              String "out",Float 201.0,
    --              Int 4,Int 2,
    --              Int 1000,Int (-1),
    --              String "para4",Int 5,
    --              String "sustain",Float 0.800000011920929,
    --              String "trig",String "c102",
    --              String "amp",String "c100",
    --              String "freq",String "c101",
    --              String "out",Float 104.0,
    --              Int 1001,Int (-1),
    --              String "para4",Int 5,
    --              String "sustain",Float 2.0,
    --              String "trig",String "c202",
    --              String "amp",String "c200",
    --              String "freq",String "c201",
    --              String "out",Float 204.0,
    --              Int 5,Int 2,
    --              Int 1006,Int (-1),
    --              String "simpleReverb",Int 4,
    --              String "damp",Float 0.5,
    --              String "room",Float 0.8999999761581421,
    --              String "mix",Float 0.5,
    --              String "in",Float 104.0,
    --              Int 1007,Int (-1),
    --              String "simpleReverb",Int 4,
    --              String "damp",Float 0.8999999761581421,
    --              String "room",Float 0.5,
    --              String "mix",Float 0.5,
    --              String "in",Float 204.0,
    --              Int 6,Int 2,
    --              Int 1008,Int (-1),
    --              String "simplePanGain",Int 3,
    --              String "pan",Float (-0.800000011920929),
    --              String "gain",Float 1.0,
    --              String "bus",Float 104.0,
    --              Int 1009,Int (-1),
    --              String "simplePanGain",Int 3,
    --              String "pan",Float 0.800000011920929,
    --              String "gain",Float 1.0,
    --              String "bus",Float 204.0]

oscList2 :: OSC
oscList2 = 
    Message "/g_queryTree.reply" 
                [Int 1,Int 0,Int 1,
                 Int 1,Int 5,
                 Int 2,Int 0,
                 Int 3,Int 4,
                 Int 1002,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c103",
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
      f (SynthNode _ n _) = [n]
      f a = []

-- | Another test for syb.
getAllStrings :: Data a => a -> [String]
getAllStrings  = everything (++) ([] `mkQ` f)
    where
      f (String s) = [s]
      f _ = []