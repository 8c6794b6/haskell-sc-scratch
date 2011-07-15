------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Commands for scsynth interactive shell.
--
module Sound.SC3.Lepton.CLI.SCShellCmd
  ( Cmd(..)
  , cmdToOSC
  ) where

import Data.Maybe (fromMaybe)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import Sound.SC3.Lepton.CLI.SCZipper

-- | Commands for sc shell.
data Cmd = Pwd                                       -- ^ view current status
         | Ls [Step]                                 -- ^ view node list
         | Tree Bool [Step]                          -- ^ view node tree
         | Cd [Step]                                 -- ^ move current focus
         | Mv AddAction NodeId NodeId                -- ^ move nodes
         | Refresh                                   -- ^ refresh synth tree
         | Status                                    -- ^ show server status
         | Set (Maybe NodeId) [SynthParam]           -- ^ n_set
         | Run Bool                                  -- ^ n_run
         | Free [NodeId]                             -- ^ n_free
         | Gnew [(NodeId, Maybe (AddAction,NodeId))] -- ^ g_new
         | Snew String NodeId (Maybe (AddAction,NodeId)) [SynthParam] -- ^ s_new
         deriving (Eq,Show)

-- | Converts given command to list of OSC with using given zipper.
cmdToOSC :: Cmd -> SCZipper -> [OSC]
cmdToOSC c z =
  let tid = nodeId $ focus z
  in  case c of
    Status               -> [Message "/status" []]
    Mv a i j             -> [Message "/n_order" [Int (fromEnum a),Int j,Int i]]
    Set n ps | null ps   -> []
             | otherwise -> mkNSet (fromMaybe tid n) ps
    Run r                -> [n_run [(tid, r)]]
    Free ns | null ns    -> []
            | otherwise  -> [n_free ns]
    Gnew ps              -> mkGNew tid ps
    Snew n i Nothing ps  -> mkSNew i AddToTail tid n ps
    Snew n i (Just (a,j)) ps -> mkSNew i a j n ps
    _                    -> []

mkNSet :: Int -> [SynthParam] -> [OSC]
mkNSet i ps = nSets i vs ++ nMaps i cs ++ nMapas i as where
  (vs,cs,as) = breakParams ps

mkGNew :: Int -> [(NodeId, Maybe (AddAction, NodeId))]  -> [OSC]
mkGNew j ps = [g_new (map f ps)] where
  f (i,(Just (a,j'))) = (i,a,j')
  f (i,Nothing)       = (i,AddToTail,j)

mkSNew :: Int -> AddAction -> Int -> String -> [SynthParam] -> [OSC]
mkSNew i a j n ps = sNews i n a j vs ++ nMaps i cs ++ nMapas i as where
  (vs,cs,as) = breakParams ps

breakParams :: [SynthParam] -> ([SynthParam],[SynthParam],[SynthParam])
breakParams ps = foldr f ([],[],[]) ps where
  f p (vs,cs,as) = case p of
    (_:=_)  -> (p:vs,cs,as)
    (_:<-_) -> (vs,p:cs,as)
    (_:<=_) -> (vs,cs,p:as)

sNews :: Int -> String -> AddAction -> Int -> [SynthParam] -> [OSC]
sNews i n a j ps = [s_new n i a j $ foldr f [] ps]
  where f p vs = case p of (k:=v) -> (k,v):vs; _ -> vs

nSets :: Int -> [SynthParam] -> [OSC]
nSets i =
  mapping (n_set i) (\p vs -> case p of (n:=v) -> (n,v):vs; _ -> vs)

nMaps :: Int -> [SynthParam] -> [OSC]
nMaps i =
  mapping (n_map i) (\p vs -> case p of (n:<-v) -> (n,v):vs; _ -> vs)

nMapas :: Int -> [SynthParam] -> [OSC]
nMapas i =
  mapping (n_mapa i) (\p vs -> case p of (n:<=v) -> (n,v):vs; _ -> vs)

mapping :: ([b] -> a) -> (c -> [b] -> [b]) -> [c] -> [a]
mapping n f ps | null ps   = []
               | otherwise = [n $ foldr f [] ps]
