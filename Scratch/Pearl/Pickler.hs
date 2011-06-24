------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
--
module Pearl.Pickler where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

--
-- From /Figure 2/, shown in code as CorePickle module
--

type St s = ([Char], s)
data PU a p = PU { appP :: (a,St p) -> St p
                 , appU :: St p -> (a,St p) }

pickle :: PU a s -> s -> a -> String
pickle p s value = fst $ appP p (value,([],s))

unpickle :: PU a s -> s -> String -> a
unpickle p s cs = fst $ appU p (cs, s)

base :: Int
base = 256

belowBase :: PU Int p
belowBase = PU (\(n,(cs,s)) -> (toEnum n : cs, s))
               (\(c:cs,s) -> (fromEnum c, (cs,s)))

lift :: a -> PU a s
lift x = PU snd (\s -> (x,s))

sequ :: (b->a) -> PU a s -> (a -> PU b s) -> PU b s
sequ f pa k =
  PU (\(b,(cs,s)) -> let a = f b
                         pb = k a
                         (cs'',s'') = appP pb (b, (cs,s'))
                         (cs',s') = appP pa (a, (cs'',s))
                     in  (cs',s''))
      (\s -> let (a,s') = appU pa s
             in  appU (k a) s')

useState :: (a->s->s) -> (s->PU a s) -> PU a s
useState update spa =
  PU (\(x,(cs,s)) -> let (cs',s') = appP (spa s) (x,(cs,s))
                     in  (cs',update x s'))
     (\(cs,s) -> let (x,(cs',s')) = appU (spa s) (cs,s)
                 in  (x,(cs',update x s')))

share :: Eq a => PU a [a] -> PU a [a]
share p = useState add (\dict -> tokenize dict p)

shareFst :: Eq a => PU a ([a],s) -> PU a ([a],s)
shareFst p =
  useState (\x -> \(s1,s2) -> (add x s1, s2))
           (\(s1,s2) -> tokenize s1 p)

shareSnd :: Eq a => PU a (s,[a]) -> PU a (s,[a])
shareSnd p =
  useState (\x -> \(s1,s2) -> (s1, add x s2))
           (\(s1,s2) -> tokenize s2 p)

add :: Eq a => a -> [a] -> [a]
add x d = if elem x d then d else x:d

unit :: PU () s
unit = lift ()

bool :: PU Bool s
bool = wrap (toEnum, fromEnum) (zeroTo 1)

char :: PU Char s
char = wrap (toEnum, fromEnum) (zeroTo 255)

string :: PU String s
string = list char

nat :: PU Int s
nat =
  sequ (\x -> if x < half then x else half + x `mod` half)
    belowBase
      (\lo -> if lo < half then lift lo
                else wrap (\hi->hi*half+lo, \n->n`div`half-1) nat)
  where half = base `div` 2

zeroTo :: Int -> PU Int s
zeroTo 0 = lift 0
zeroTo n =
  wrap (\(hi,lo) -> hi*base+lo, (`divMod` base))
    (pair (zeroTo (n `div` base)) belowBase)

pair :: PU a s -> PU b s -> PU (a,b) s
pair pa pb =
  sequ fst pa $ \a ->
  sequ snd pb $ \b ->
  lift (a,b)

triple :: PU a s -> PU b s -> PU c s -> PU (a,b,c) s
triple pa pb pc =
  sequ (\(x,_,_) -> x) pa $ \a ->
  sequ (\(_,x,_) -> x) pb $ \b ->
  sequ (\(_,_,x) -> x) pc $ \c ->
  lift (a,b,c)

quad :: PU a s -> PU b s -> PU c s -> PU d s -> PU (a,b,c,d) s
quad pa pb pc pd =
  sequ (\(x,_,_,_) -> x) pa $ \a ->
  sequ (\(_,x,_,_) -> x) pb $ \b ->
  sequ (\(_,_,x,_) -> x) pc $ \c ->
  sequ (\(_,_,_,x) -> x) pd $ \d ->
  lift (a,b,c,d)

pMaybe :: PU a s -> PU (Maybe a) s
pMaybe pa = alt tag [lift Nothing, wrap (Just, fromJust) pa] where
  tag Nothing  = 0
  tag (Just _) = 1

pEither :: PU a s -> PU b s -> PU (Either a b) s
pEither pa pb = alt tag [wrapL, wrapR] where
  tag (Left _) = 0
  tag (Right _) = 0
  wrapL = wrap (Left, \(Left x) -> x) pa
  wrapR = wrap (Right, \(Right x) -> x) pb

fixedList :: PU a s -> Int -> PU [a] s
fixedList pa 0 = lift []
fixedList pa n =
  wrap (\(a,b) -> a:b, \(a:b) -> (a,b)) (pair pa (fixedList pa (n-1)))

list :: PU a s -> PU [a] s
list = sequ length nat . fixedList

wrap :: (a->b,b->a) -> PU a s -> PU b s
wrap (i,j) pa = sequ j pa (lift . i)

alt :: (a->Int) -> [PU a s] -> PU a s
alt tag ps = sequ tag (zeroTo (length ps-1)) (ps !!)

tokenize :: Eq a => [a] -> PU a s -> PU a s
tokenize dict p =
  sequ (\x -> case elemIndex x dict of
           Just i  -> n-i
           Nothing -> 0)
    (zeroTo n) (\i -> if i == 0 then p else lift (dict!!(n-1)))
  where n = length dict

memoFixedList :: Eq a => [a] -> PU a s -> Int -> PU [a] s
memoFixedList dict pa 0 = lift []
memoFixedList dict pa n =
  sequ head (tokenize dict pa) $ \x ->
  sequ tail (memoFixedList (add x dict) pa (n-1)) $ \xs ->
  lift (x:xs)

memoList :: Eq a => [a] -> PU a s -> PU [a] s
memoList dict = sequ length nat . memoFixedList dict

--
-- Data types used for, application like web browser.
--
-- In easy way, Type synonyms could be defined as below:
--
-- > type URL = (String, String, Maybe Int, String)
-- > type Bookmark = (String, URL)
--
-- But not this time.
--

data URL = URL { protocol :: String
               , host :: String
               , port :: Maybe Int
               , file :: String }
           deriving (Eq, Show)

data Bookmark = Link (String, URL)
              | Folder (String, Bookmarks)
                deriving (Eq, Show)

type Bookmarks = [Bookmark]

url :: PU URL s
url = wrap (\(pr,h,po,f) -> URL pr h po f,
            \(URL pr h po f) -> (pr,h,po,f))
      (quad string string (pMaybe nat) string)

bookmark :: PU Bookmark s
bookmark =
  alt tag [wrap (Link, \(Link a) -> a) (pair string url)
          ,wrap (Folder, \(Folder a) -> a) (pair string bookmarks)] where
    tag (Link _) = 0
    tag (Folder _) = 0

bookmarks :: PU Bookmarks s
bookmarks = list bookmark

data Lambda = Var String
            | Lam (String, Lambda)
            | App (Lambda, Lambda)
            deriving (Eq, Show)

tag_lambda :: Lambda -> Int
tag_lambda (Var _) = 0
tag_lambda (Lam _) = 1
tag_lambda (App _) = 2

lambda :: PU Lambda s
lambda =
  alt tag_lambda [ wrap (Var, \(Var x) -> x) string
                 , wrap (Lam, \(Lam x) -> x) (pair string lambda)
                 , wrap (App, \(App x) -> x) (pair lambda lambda) ]

lambda2 :: PU Lambda s
lambda2 =
  wrap (sumlam, lamsum)
    (pEither string (pEither (pair string lambda) (pair lambda lambda)))
  where
    lamsum (Var x) = Left x
    lamsum (Lam x) = Right (Left x)
    lamsum (App x) = Right (Right x)
    sumlam (Left x)          = Var x
    sumlam (Right (Left x))  = Lam x
    sumlam (Right (Right x)) = App x

slambda = share $ alt tag_lambda
  [ wrap (Var, \(Var x) -> x) string
  , wrap (Lam, \(Lam x) -> x) (pair string slambda)
  , wrap (App, \(App x) -> x) (pair slambda slambda) ]

lambda3 = shareFst $ alt tag_lambda
  [ wrap (Var, \(Var x) -> x) var
  , wrap (Lam, \(Lam x) -> x) (pair var lambda3)
  , wrap (App, \(App x) -> x) (pair lambda3 lambda3) ]
  where var = shareSnd string

b01 :: Bookmarks
b01 =
  [ Link ("Andrew",
          URL { protocol = "http"
              , host = "research.microsoft.com"
              , port = Nothing
              , file = "users/akenn" })]

kki = App (k, App (k, i)) where
  k = Lam ("x", Lam ("y", x))
  i = Lam ("x", x)
  x = Var "x"