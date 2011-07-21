module Test.Sound.SC3.Lepton.CLI.SCShellCmd where

import Test.QuickCheck

import Sound.SC3.Lepton
import Sound.SC3.Lepton.CLI.SCShellCmd
-- import Sound.SC3.Lepton.CLI.SCZipper
import Test.Sound.SC3.Lepton.CLI.Common ()

runTests :: IO [Result]
runTests =
  mapM quickCheckResult
    [label "cmdInstance" prop_cmdInstance
    ,label "cmdToOSC" prop_cmdToOSC]

prop_cmdInstance :: Property
prop_cmdInstance =
  forAll (arbitrary :: Gen Cmd) $ \c -> c == c && show c == show c

prop_cmdToOSC :: Cmd -> SCZipper -> Property
prop_cmdToOSC c z =
  let o = cmdToOSC c z
      l0 = length o == 0
  in  collect o $ case c of
     Ls _     -> l0
     Pwd      -> l0
     Tree _ _ -> l0
     Cd _     -> l0
     Refresh  -> l0
     Set _ [] -> l0
     Free []  -> l0
     _        -> length o > 0
