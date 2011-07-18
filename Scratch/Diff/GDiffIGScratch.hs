{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module GDiffIGScratch where

import Generics.Instant.GDiff
import Generics.Instant.TH
import Generics.Instant.Functions.Show
import Sound.SC3.Lepton
import Sound.SC3.Lepton.QuickCheck

import Sample

$(deriveAll ''SCNode)
$(deriveAll ''SynthParam)

level :: SCNode -> Int
level (Synth _ _ _) = 0
level (Group _ ns)  = 1 + maximum (map level ns)

instance GShow SCNode where gshow = gshow 
instance SEq SCNode where shallowEq = shallowEqDef  
instance Build SCNode where build = buildDef
instance Children SCNode where children = childrenDef                            
instance GDiff SCNode                               

instance GShow SynthParam where gshow = gshow 
instance SEq SynthParam where shallowEq = shallowEqDef  
instance Build SynthParam where build = buildDef
instance Children SynthParam where children = childrenDef                            
instance GDiff SynthParam

instance SEq Double where shallowEq = (==)
instance Build Double
instance Children Double
instance GDiff Double

-- instance GShow SynthParam where gshow = gshow 
-- instance SEq SynthParam where shallowEq = shallowEqDef  
-- instance Build SynthParam where build = buildDef
-- instance Children SynthParam where children = childrenDef                            
-- instance GDiff SynthParam


