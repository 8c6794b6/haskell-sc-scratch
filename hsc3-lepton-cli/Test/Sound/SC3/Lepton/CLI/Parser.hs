module Test.Sound.SC3.Lepton.CLI.Parser where

import Control.Applicative
import Data.List (intercalate,intersperse)
import Test.QuickCheck

import Sound.SC3.Lepton.CLI.Parser
import Sound.SC3.Lepton.CLI.SCShellCmd

runTests :: IO [Result]
runTests =
  mapM (quickCheckWithResult stdArgs {maxSuccess=250, maxDiscard=1000})
    [label "parseCmd" prop_parseCmd]

prop_parseCmd :: Property
prop_parseCmd =
  forAll gen_commands $ \str ->
  let result = parseCmd str
  in  case result of
    Left  _ -> collect result $ False
    Right c -> collect (cmdName c) $ True

cmdName :: Cmd -> String
cmdName c = case c of
  Pwd          -> "pwd"
  Ls _         -> "ls"
  Cd _         -> "cd"
  Mv _ _ _     -> "mv"
  Tree _       -> "tree"
  Status       -> "status"
  Refresh      -> "refresh"
  Set _ _      -> "set"
  Run _        -> "run"
  Free _       -> "free"
  Snew _ _ _ _ -> "snew"
  Gnew _       -> "gnew"

gen_commands :: Gen String
gen_commands = oneof
  [gen_pwd, gen_status, gen_refresh, gen_ls, gen_cd, gen_tree, gen_set, gen_run
  ,gen_free, gen_snew, gen_gnew, gen_mv]

gen_pwd :: Gen String
gen_pwd = addSpaces $ return "pwd"

gen_ls :: Gen String
gen_ls = addSpaces $ ("ls " ++) <$> gen_path

gen_tree :: Gen String
gen_tree = addSpaces $ ("tree " ++) <$> addSpaces gen_path

gen_refresh :: Gen String
gen_refresh = addSpaces $ return "refresh"

gen_status :: Gen String
gen_status = addSpaces $ return "status"

gen_cd :: Gen String
gen_cd = addSpaces $ ("cd " ++) <$> gen_path

gen_set :: Gen String
gen_set = addSpaces $ do
  nid <- oneof [show <$> (arbitrary :: Gen Int), return ""]
  ps <- listOf1 gen_synthParam
  return $ intercalate " " ("set":nid:ps)

gen_run :: Gen String
gen_run = addSpaces $ ("run " ++) <$>
  elements ["t", "T", "1", "on", "f", "F", "0", "off"]

gen_free :: Gen String
gen_free = addSpaces $ do
  nids <- listOf1 (arbitrary :: Gen Int)
  return $ "free " ++ (intercalate " " $ map show nids)

gen_nid :: Gen String
gen_nid =
  oneof [show <$> ((arbitrary :: Gen Int) `suchThat` (> 0)), return "-1"]

gen_nidWithAA :: Gen String
gen_nidWithAA = do
  nid <- oneof [show <$> ((arbitrary :: Gen Int) `suchThat` (> 0)),return "-1"]
  aa <- elements ["a","b","h","t","r"]
  tid <- show <$> ((arbitrary :: Gen Int) `suchThat` (>0))
  elements [nid, nid ++ aa ++ tid]

gen_snew :: Gen String
gen_snew = addSpaces $ do
  defname <- gen_generalName
  nid <- gen_nidWithAA
  ps <- listOf gen_synthParam
  return $ intercalate " " ("snew" : defname : nid : ps)

gen_gnew :: Gen String
gen_gnew = addSpaces $ do
  nids <- listOf1 gen_nidWithAA
  return $ intercalate " " ("gnew":nids)

gen_mv :: Gen String
gen_mv = addSpaces $ do
  aa <- elements ["-h", "--head", "-t", "--tail"
                 ,"-a", "--after", "-b", "--before"]
  nid1 <- show <$> (arbitrary :: Gen Int)
  nid2 <- show <$> (arbitrary :: Gen Int)
  return $ intercalate " " ["mv", aa, nid1, nid2]

gen_path :: Gen String
gen_path = do
  path <- listOf $ oneof [show <$> (arbitrary::Gen Int), return ".."]
  beginningSlash <- elements ["/", ""]
  endingSlash <- elements ["/", ""]
  return $ intercalate "/" $ beginningSlash : path ++ [endingSlash]

gen_synthParam :: Gen String
gen_synthParam = do
  gname <- gen_generalName
  valuePrefix <- elements ["a", "c", ""]
  value <- if not (null valuePrefix)
             then show <$> (arbitrary :: Gen Int) `suchThat` (> 0)
             else show <$> (arbitrary :: Gen Double)
  return $ gname ++ "=" ++ valuePrefix ++ value

gen_generalName :: Gen String
gen_generalName = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "._"

addSpaces :: Gen String -> Gen String
addSpaces g = do
  rightSpaces <- listOf $ elements " "
  leftSpaces <- listOf $ elements " "
  middle <- g
  return $ rightSpaces ++ middle ++ leftSpaces

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
