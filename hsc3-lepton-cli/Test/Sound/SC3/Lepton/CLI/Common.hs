module Test.Sound.SC3.Lepton.CLI.Common where

import Test.QuickCheck

import Control.Applicative
import Sound.SC3.Lepton
import Sound.SC3.Lepton.QuickCheck ()

import Sound.SC3.Lepton.CLI.SCShellCmd

class Sizeable n where
  sizeOf :: n -> Int

instance Sizeable SCZipper where
  sizeOf (SCZipper n ps) = sizeOf n + sizeOf ps

instance Sizeable SCPath where
  sizeOf (SCPath _ ls rs) = 1 + sizeOf ls + sizeOf rs

instance Sizeable SCNode where
  sizeOf (Group _ ns) = 1 + sizeOf ns
  sizeOf (Synth _ _ _) = 1

instance Sizeable a => Sizeable [a] where
  sizeOf = sum . map sizeOf

instance Arbitrary Cmd where
  arbitrary = oneof
    [Ls <$> arbitrary
    ,Cd <$> arbitrary
    ,Mv <$> arbitrary <*> arbitrary <*> arbitrary
    ,Tree <$> arbitrary <*> arbitrary
    ,return Status
    ,return Refresh
    ,Set <$> arbitrary <*> arbitrary
    ,Run <$> arbitrary
    ,Free <$> arbitrary
    ,Gnew <$> arbitrary
    ,Snew <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]

instance CoArbitrary Cmd where
  coarbitrary c = case c of
    Ls ps -> variant (1::Int) . coarbitrary ps
    _     -> undefined

isSuccess :: Result -> Bool
isSuccess r = case r of Success _ _ _ -> True; _ -> False;
