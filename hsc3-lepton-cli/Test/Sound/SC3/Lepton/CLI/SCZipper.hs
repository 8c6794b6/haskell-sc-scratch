module Test.Sound.SC3.Lepton.CLI.SCZipper where

import Text.Show.Functions ()
import Test.QuickCheck

import Sound.SC3
import Sound.SC3.Lepton

import Sound.SC3.Lepton.CLI.SCZipper
import Test.Sound.SC3.Lepton.CLI.Common

import qualified Data.IntSet as IS

runTests :: IO [Result]
runTests =
  -- SCZipper test data gets too large for prop_goTop
  -- modify maxSize from default 100.
  mapM (quickCheckWithResult (stdArgs {maxSize=25}))
    [label "updateNode" prop_updateNode
    ,label "zipperInstance" prop_zipperInstance
    ,label "goTop" prop_goTop
    ,label "steps" prop_steps
    ,label "delete" prop_delete
    ,label "insert" prop_insert
    ,label "insert_delete" prop_insert_delete]

prop_updateNode :: (SCNode -> SCNode) -> SCZipper -> Property
prop_updateNode f z =
  let newSize = sizeOf (updateNode f z)
      oldSize = sizeOf z
  in  collect (newSize `compare` oldSize) $ True

prop_zipperInstance :: Property
prop_zipperInstance =
  forAll (arbitrary :: Gen SCZipper) $ \z -> z == z && show z == show z

prop_steps :: [Step] -> SCZipper -> Property
prop_steps s z =
  not (null s) ==>
  let z' = steps s z
  in  sizeOf (focus z') >= 0 || sizeOf (focus z') < 0

prop_goTop :: SCZipper -> Property
prop_goTop z =
  not (null $ scPaths z) ==>
  forAll (elements $ scPaths z) $ \p ->
  focus (goTop z) == focus (goTop (goDown (nodeIdOfPath p) z))

prop_delete :: SCZipper -> Property
prop_delete z =
  not (null $ scPaths z) ==>
  forAll (elements $ scPaths z) $ \p ->
  not (null $ leftPaths p ++ rightPaths p) ==>
  forAll (elements $ leftPaths p ++ rightPaths p) $ \n ->
  sizeOf z > sizeOf (delete (nodeId n) z)

prop_insert :: AddAction -> Property
prop_insert a =
  forAll (gen_uniqueZipper `suchThat` (not . null . scPaths)) $ \z ->
  forAll gen_uniqueNodeId $ \n ->
  forAll (elements $ nodeIdsInZipper z) $ \targetId ->
    IS.size (IS.fromList (nodeIdsInZipper z) `IS.intersection`
             IS.fromList (nodeIds n)) == 0 ==>
    nodeIdOfPath (head (scPaths z)) /= targetId ==>
    collect a $
    let z' = insert' n (Just (a,targetId)) z
    in case a of
      AddReplace ->
        sizeOf (nodeById targetId z) + sizeOf z' == sizeOf n + sizeOf z
      _          ->
        (sizeOf z + sizeOf n == sizeOf z') || (z == z')

-- | Insert and then delete the same node. This is doing almost same thing
-- as @move@ function with AddToTail add action.
prop_insert_delete :: Property
prop_insert_delete =
  forAll gen_uniqueZipper $ \z ->
  forAll gen_uniqueNodeId $ \n ->
  classify (isSynth n) "synth" $
  classify (isGroup n) "group" $
  let z' = insert n z
      everyNodesAreUnique =
        IS.size (IS.fromList (f z) `IS.intersection`
                 IS.fromList (nodeIds n)) == 0
      f (SCZipper n' ps) = nodeIds n' ++ concatMap g ps
      g (SCPath n' ls rs) = n' : concatMap nodeIds ls ++ concatMap nodeIds rs
  in  everyNodesAreUnique ==> z == delete (nodeId n) z'

-- Scratch with:
--
-- * http://stackoverflow.com/questions/2838670/how-to-functionally-generate-a-tree-breadth-first-with-haskell
--
-- kids :: String -> [String]
-- kids x = [x ++ "a", x ++ "bb"]

-- expand :: MonadState Bool m => String -> String -> m (String, [String])
-- expand query x = do
--   found <- get
--   if found
--      then return (x, [])
--      else do
--        let (before, after) = break (== query) $ kids x
--        if null after
--           then return (x,before)
--           else do
--             put True
--             return (x, before ++ [head after])

-- searchBF :: String -> T.Tree String
-- searchBF query = (evalState $ T.unfoldTreeM_BF (expand query) []) False

-- -- | Try:
-- --
-- -- > > printSearchBF "aabb"
-- -- > > printSearchBF "abba"
-- --
-- printSearchBF :: String -> IO ()
-- printSearchBF = putStrLn . T.drawTree . searchBF


-- z0 = SCZipper n [] where
--   n = Group 0
--        [Group 1 []
--        ,Synth 2 "a" ["821,@#0":=38298,"*SDKJE*j8u78":=8]
--        ,Group 3
--          [Synth 31 "b" ["foo":<-3,"bar":=33]
--          ,Synth 32 "c" ["askdfjk":<=(-983),"asooiuq":=8298]
--          ,Group 33
--             [Synth 331 "e" ["fdai":=832,"iwans832":=38981,"swu":=3]
--             ,Synth 332 "f" ["dkj":=8,"asui":=3829]
--             ,Group 333 []] ]
--        ,Synth 4 "g" ["iu":=8,"iu28nzz[":=8]
--        ,Group 5 []]

-- n0 = Group 9999 [Synth 9998 "newnode" []]

-- z1 = SCZipper
--   {focus = Synth (-61) "rAkEAzFbTdv"
--            ["X":<-(-111),"Jayd_mBqq":=(-20.69),"CO.yDnC":=8.16,"sQV":=7.41
--            ,"_ngJ":=(-8.19),"PthRfSO":<-(-28),"azpesEIf":=0.24
--            ,"hEB":<-52,"iSNd":=0.42,"Bf.WGUG":<-(-62),"HLuGuyuXVZEJ":<-70,"vKe_":<=7]
--   , scPaths = [SCPath (-114)
--                [Group (-26) [Group 53 []]
--                ,Synth 33 "nDPizyAcHTU"
--                   ["stBs":<=(-73),"bSOeoKTiqUMY":<-(-52)
--                   ,"xfAokC":=(-20.90),"HSKicl":=35.24,"wbLhDFyY":<=42]
--                ,Group (-17) [Synth 86 "dNZMV" [],
--                              Group 106 [Group (-117) [Group 75 []
--                                                      ,Group 120 [Synth 95 "yC.wPMq" []]]
--                                        ,Group (-125) [Synth 114 "DSqYhO" ["fTIszLs":<=23,"BVOC":<-24]
--                                                      ,Synth (-29) "f" ["bjnPgGPp":<-116
--                                                                       ,"L_nlZFGMPnAl":<=(-57)
--                                                                       ,"hDHeZsZ":<=119
--                                                                       ,"nUA":<-13
--                                                                       ,"BMAWpgQ":=24.12]]]]
--                ,Group (-67) []
--                ,Synth (-111) "aCoO" ["NJw":<=(-101),"uHsIUiXx":<=39]
--                ,Synth (-47) "Pqv" ["SVJVPA":=26.82]
--                ,Synth 9 ".wRGkM" ["dkQhav":=(-11.24),"RcD":=(-8.41)
--                                  ,"KV":<=36,"ENq":<=(-7),"rGn..X":=15.68
--                                  ,"k":<=(-70),"hpWACGi":<-(-114),"RoYBfXxk_":=50.28
--                                  ,"QkZe":=4.10,"SsAwLoZp":<-29]]
--                [Synth 61 "WgNAB" ["NimAMQHNMm":<-112,"RxYMpr":=(-6.14)]
--                ,Synth 20 "nVgbX_XJBcBs" ["qJrZkPpDYQ":=27.38,"nGBkpf":<-128
--                                         ,"kNMoz":=(-7.07),"xY":<=123
--                                         ,"GcaeHkCifWy":<=(-76),"EmANCuvsatLg":<=(-98)
--                                         ,"UTlISlE":=(-44.94),"Awg":<-(-120)
--                                         ,"erq":=14.46,"aFf":<=70]]]}

-- n1 = Synth (-14) "CUEitMBoT" []

-- targetId = (-114)
