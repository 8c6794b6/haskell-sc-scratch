------------------------------------------------------------------------------
-- | 
-- Module for handling phrases.
-- 

module Sound.SC3.Wing.Phrase where

import Control.Applicative
import Data.List (transpose)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Wing.Schedule (Event, listE)
import Sound.SC3.Wing.Tree (NodeId)

-- | Type synonym for phrase.
type Phrase = Map String [Double]

-- | Plays percussive phrase.
-- 
-- Key of the Phrase would be used as name of synthdef, @[Double]@
-- values would be used as parameter for @amp@ argument of synthdef.
-- 0 values @amp@ would be rest.
-- 
mkPerc :: NodeId -- ^ Node id of group 
       -> [Double] -- ^ duration
       -> Phrase -- ^ Map to be used as phrase.
       -> Event OSC
mkPerc gId durs phr = mconcat $ M.elems $ M.map (mkEvent durs) $
                      M.mapWithKey (map . mkPerc') phr
    where
      mkEvent ds os = listE $ catMaybes $ zipWith liftPair ds os
      liftPair a b = pure (,) <*> pure a <*> b
      mkPerc' n a 
              | a > 0 = Just $ s_new n (-1) AddToTail gId
                        [("amp", a)]
              | otherwise = Nothing

-- | Makes list of s_new osc messages from Phrase.
mkSNew :: String -> NodeId -> AddAction -> NodeId -> Phrase -> [OSC]
mkSNew name nodeId addAction targetId =
  map (s_new name nodeId addAction targetId) . transposeWithKeys

-- | Variant of mkSNew with nodeId (-1) and AddToTail.
mkSNew' :: String -> NodeId -> Phrase -> [OSC]
mkSNew' name targetId = mkSNew name (-1) AddToTail targetId

-- | Makes list of n_set osc messages from Phrase.
mkNSet :: NodeId -> Phrase -> [OSC]
mkNSet nId = map (n_set nId) . transposeWithKeys

transposeWithKeys :: Map a [b] -> [[(a,b)]]
transposeWithKeys =
  transpose . M.elems . M.mapWithKey (\k a -> zip (repeat k) a)

newEvent :: String -> NodeId -> Phrase -> Event OSC
newEvent name gId phr = undefined

setMonoEvent :: NodeId -> Phrase -> Event OSC
setMonoEvent nid phr = mappend (listE $ zip dur osc)
                 (listE $ zipWith closeGate dur sus)
    where
      dur = scanl (+) 0 $ maybe [] id $ M.lookup "dur" phr
      osc = mkNSet nid $ M.insert "gate" (repeat 1) $ phr
      sus = maybe [] id . M.lookup "sustain" $ phr
      closeGate d o = ((d+o), n_set nid [("gate",0)])
