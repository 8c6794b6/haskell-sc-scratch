------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- A very simple file system.
--
module LearnYour.Zipper.FS where

import Data.List (break)

import LearnYour.Zipper.Tree

type Name = String
type Data = String

data FSItem = File Name Data
            | Folder Name [FSItem]
            deriving (Eq, Show)

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "goat_yelling_like_man.wmv" "baaaaa"
    , File "pope_time.avi" "god bless"
    , Folder "pics"
        [ File "ape_throwing_up.jpg" "bleargh"
        , File "watermelon_smash.gif" "smash!!"
        , File "skull_man(scary).bmp" "Yikes!" ]
    , File "dijon_poupon.doc" "best musterd"
    , Folder "programs"
      [ File "fartwizard.exe" "10gotofart"
      , File "owl_bandit.dmg" "mov eax, h00t"
      , Folder "source code"
        [ File "best_hs_prog.hs" "main = print (fix error)"
        , File "random.hs" "main = print 4" ]]]

data FSCrumb = FSCrumb Name [FSItem] [FSItem]
               deriving (Eq, Show)

crumbL (FSCrumb _ ls _) = ls
crumbR (FSCrumb _ _ rs) = rs

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb n ls rs:bs) = (Folder n (ls++[item]++rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folder items, bs) = (item, FSCrumb folder ls rs:bs) where
  (ls, item:rs) = break (nameIs name) items
  nameIs name (Folder name' _) = name == name'
  nameIs name (File name' _)   = name == name'

fsRename :: Name -> FSZipper -> FSZipper
fsRename name (Folder _ is, bs) = (Folder name is, bs)
fsRename name (File _ dat, bs)  = (File name dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile i (Folder name is, bs) = (Folder name (i:is), bs)
