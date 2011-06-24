------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Pearl.HuetZipper where

data Tree a = Item a
            | Section [Tree a]
            deriving (Eq, Show)

data Path a = Top
            | Node [Tree a] (Path a) [Tree a]
            deriving (Eq, Show)

data Location a = Loc (Tree a) (Path a)
                  deriving (Eq, Show)

t01 :: Tree String
t01 =
  Section
    [Section
       [Item "a", Item "*", Item "b"]
    ,Item "+"
    ,Section
       [Item "c", Item "*", Item "d"]]

l01 :: Location String
l01 =
  Loc (Item "*")
    (Node
      [Item "c"]
      (Node [Item "+"
            ,Section [Item "a"
                     ,Item "*"
                     ,Item "b" ]]
            Top
            [])
      [Item "d"])

go_left :: Location a -> Location a
go_left (Loc t p) = case p of
  Top                   -> error "Left of top"
  Node [] _ _           -> error "Left of first"
  Node (l:ls) up rs -> Loc l (Node ls up (t:rs))

go_right :: Location a -> Location a
go_right (Loc t p) = case p of
  Top               -> error "Right of top"
  Node _ _ []       -> error "Right of first"
  Node ls up (r:rs) -> Loc r (Node (t:ls) up rs)

go_up :: Location a -> Location a
go_up (Loc t p) = case p of
  Top         -> error "Up of top"
  Node l up r -> Loc (Section $ reverse l ++ (t:r)) up

go_down :: Location a -> Location a
go_down (Loc t p) = case t of
  Item _         -> error "Down of item"
  Section (t:ts) -> Loc t (Node [] p ts)
  _              -> error "Down of empty"

to_top :: Location a -> Location a
to_top (l@(Loc t p)) = case p of
  Top -> Loc t p
  Node _ _ _ -> to_top $ go_up l

rootNode :: Location a -> Tree a
rootNode = (\(Loc t _) -> t) . to_top

x -: f = f x

nth :: Location a -> Int -> Location a
nth loc = nthrec where
  nthrec 1 = go_down loc
  nthrec n = if n > 0
               then go_right (nthrec (n-1))
               else error "nth expects positive integer"

change :: Location a -> Tree a -> Location a
change (Loc _ p) t = Loc t p

insert_right :: Location a -> Tree a -> Location a
insert_right (Loc t p) e = case p of
  Top                -> error "Insert of top"
  Node left up right -> Loc t (Node left up (e:right))

insert_left :: Location a -> Tree a -> Location a
insert_left (Loc t p) e = case p of
  Top                -> error "Insert of top"
  Node left up right -> Loc t (Node (e:left) up right)

insert_down :: Location a -> Tree a -> Location a
insert_down (Loc t p) e = case t of
  Item _ -> error "Down of item"
  Section es -> Loc e (Node [] p es)

delete :: Location a -> Location a
delete (Loc _ p) = case p of
  Top -> error "Delete of top"
  Node ls up (r:rs) -> Loc r (Node ls up rs)
  Node (l:ls) up [] -> Loc l (Node ls up [])
  Node [] up []     -> Loc (Section []) up
