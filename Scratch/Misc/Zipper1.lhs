From oleg-at-okmij.org Wed Apr 27 16:17:04 2005
To: haskell@haskell.org
Subject: Zipper as a delimited continuation
X-comment: Aug 26, 2009: implemented a different convention for the
  direction of traversal steps. It is less convenient but more
  intuitive since it takes one step to reach a parent of a subterm. The
  addendum was prompted by a conversation with Ron de Bruijn, who
  pointed out that the traversal directions in the original `traverse'
  are unintuitive. In particular, it takes more than one Up step to
  really reach a parent of a subterm.
X-comment: Feb 2011: Switching to the delimcc library
Message-ID: <20050427231704.DE8F9ABCC@Adric.metnet.navy.mil>
Date: Wed, 27 Apr 2005 16:17:04 -0700 (PDT)
Status: OR


This is the first part of a reply to a query about a zipper with two
foci, posted on this list by Oktaviandi Hadi Nugraha on Apr 13. In
this part we introduce the framework to answer the question.

Our treatment of zipper is quite different from that of Huet (JFP,
1997) and Hinze and Jeuring (JFP 2001). Our zipper is polymorphic over
the data structure to traverse, and the zipper creation procedure is
generic and does not depend on the data structure at all.  Different
data structures or different realizations of the same abstract data
structure can use the same zipper and the same zipper creation and
manipulation functions. Our zipper type depends only on the interface
(but not the implementation!) of a traversal function. Our zipper is a
derivative of a traversal function rather than that of a data
structure itself.

Zipper is a construction that lets us replace an item deep in a
complex data structure, e.g., a tree or a term, without any
mutation. The resulting data structure will share as much of its
components with the old structure as possible. The old data structure
is still available (which can be handy if we wish to 'undo' the
operation later on). Zipper is essentially an `updateable' and yet
pure functional cursor into a data structure.

Zipper is also a delimited continuation reified as a data
structure. In this message, we use delimited continuations directly to
derive the zipper. We will be relying on the delimited continuation
library delimcc, which permits single and multiple prompts.

> -- Haskell98!
> module Zipper1 where
>
> -- Import delimcc, see
> -- http://okmij.org/ftp/continuations/implementations.html#CC-monads
> import CCExc
> -- import CCCxce  -- would work as well

> import Control.Monad.Identity
> import Control.Monad.Trans


The derivation of the zipper starts with a term traversal function. The
zipper will be as good and powerful as the traversal function. Let us
adopt as a running example the familiar and dear data structure:

> data Term = Var String | A Term Term | L String Term 
>             deriving (Eq)
> instance Show Term where
>   show term = showt 0 term 
>    where showt _ (Var s)   = s
>          showt p (A e1 e2) = paren (p > 10) 
>                               (showt 10 e1 ++ " " ++ showt 11 e2)
>          showt p (L v e)   = paren (p > 0) ("L" ++ v ++ ". " ++ showt 0 e)
>          paren True  s = "(" ++ s ++ ")"
>          paren False s = s


In this message, we chose the following function to traverse the lambda-term:

> data Direction = Down | DownRight | Up | Next deriving (Eq, Show)
> traverse :: Monad m => (Term -> m (Maybe Term, Direction)) -> Term -> m Term
> traverse tf term = do
>   (term', direction) <- tf term
>   let new_term = maybe term id term'
>   let select Up t = return t
>       select Next t@(Var _) = return t
>       select dir t@(L v t1) | dir == Next || dir == Down = do
>   	  t1' <- traverse tf t1
>   	  return $ L v t1'
>       select DownRight t@(A t1 t2) = do
>   	  t2' <- traverse tf t2
>   	  return $ A t1 t2'
>       select dir t@(A t1 t2) | dir == Next || dir == Down = do
>   	  t1' <- traverse tf t1
>   	  t2' <- traverse tf t2
>   	  return $ A t1' t2'
>   select direction new_term

The function `traverse' receives the traversal function `tf' and the
lambda-term, and walks and _updates_ the term, as guided by `tf'.  The
traversal function receives the current subterm and should return a
pair. If the first component of a pair is (Just t'), then t' replaces
the current subterm. The second component of tf's result is the
direction to proceed. The direction Next means proceed in the
depth-first order. Other directions may be used to skip some parts of
the term and so avoid walking the whole term. The function is written
in a monadic style (for an arbitrary monad m). We shall need that
later.

[Addendum] 
I guess I should have used better names for Up, Down, etc. For
example, one can see from the code for 'traverse' that Up means we
are done with the term, and Next and Down are almost synonymous. To
move to the parent of a subterm, we almost always have to make two
steps Up. I guess I visualized the direction not by the destination
point but by the source point: where to go from the current term:
traverse its children or not. At the end of the file, we show
a different choice of step directions.
[/Addendum]


We will be using the following term as a running example:

> -- P2 numeral
> term1 = L "f" (L "x" (A (A f (L "f" (A f (L "f" (L "x" x)))))
> 			  (A (A f (L "f" (L "x" x))) x)))
>  where [f,x] = map Var ["f","x"]

The first test simply traverses the whole term, makes no alterations
and returns the (copy of the) term:

> testt1 = runIdentity (traverse (\term -> return (Nothing,Next)) term1)

-- *Zipper1> testt1 == term1
-- True

To make sure that we really traverse the term, we can print out all
the encountered subterms:

> testt2 = traverse tf term1
>     where tf term = print term >> return (Nothing,Next)


We instantiate `traverse' for the IO monad this time. We can skip some
parts of the term during the traversal:

> testt3 = traverse tf term1
>     where 
>     tf term@(A (Var "f") _)  = do
> 			         print "cutting" >> print term
> 			         return (Nothing,Up)
>     tf term = print term >> return (Nothing,Next)

and we can modify the term

> testt4 = runIdentity (traverse tf term1)
>     where tf (L "x" (Var "x")) = return (Just (L "y" (Var "y")),Next)
> 	    tf _ = return (Nothing,Next)

which indeed returns term1 with all occurrences of (L "x" (Var "x"))
replaced with (L "y" (Var "y")).


Now we an introduce the zipper:

> data Zipper m term dir = 
>     Zipper term ((Maybe term, dir) -> CCW m (Zipper m term dir)) 
>   | ZipDone term

The traversal monad is the CC delimcc transformer applied to the base
monad "m".  We will be using the prompt flavor PS. There is only one
prompt of this flavor, bound to the global variable |ps|, for the
fixed answer-type Zipper m term dir (the answer-type is recursive).

> type CCM m term dir a = CC (PS (Zipper m term dir)) m a 
> type CCW m w = CC (PS w) m w

Zipper is indeed polymorphic over the term to traverse (as well over the
source monad 'm'). We can use this Zipper, as it is, for _any_ data
structure that can be traversed by a function that looks like
`traverse'. 

Creating the zipper equally generic:

> zip'term :: (Monad m) =>
>   ((term -> CCM m term dir (Maybe term, dir)) -> term -> CCM m term dir term)
>   -> term -> CCW m (Zipper m term dir)
> zip'term trav term = pushPrompt ps (trav tf term >>= return . ZipDone)
>     where tf term = shift0P ps (\k -> return (Zipper term k))

Both Zipper and its creation function depend neither on the
representation of the term nor on the traversal strategy.  All the
information about the data structure and its traversal is
encapsulated in one single function `trav'.

We can now examine term1, subterm by subterm, using the cursor,
zipper, rather than the enumerator, traverse:

> zip'through (ZipDone term) = liftIO (print "Done" >> print term)
> zip'through (Zipper term k) = do
>   liftIO $ print term
>   k (Nothing,Next) >>= zip'through
> tz1 :: IO ()
> tz1 = runCC $ zip'term traverse term1 >>= zip'through

We have effectively `inverted' the operation of term traversal. With
the cursor, we can keep the arbitrary state from one traversal step to
another.

The cursor provided by zipper is updateable. We can now descend (or
walk) to the desired node, replace it, and zip up the result to yield
the updated term:

> zip'move dir (Zipper term k) = 
>   liftIO (print dir >> print term) >> k (Nothing,dir)

> zip'upr (Zipper term k) nt = do 
>   liftIO (print term >> print "replacing with" >> print nt)
>   k (Just nt,Up)
> zip'all'the'way'up (ZipDone term) = return term
> zip'all'the'way'up (Zipper term k) = 
>   k (Nothing,Up) >>= zip'all'the'way'up
>
> tz2 :: IO () 
> tz2 = runCC $ do
>   z <- zip'term traverse term1
>   z1 <- zip'move Next z
>   z1 <- zip'move Next z1
>   z2@(Zipper (A _ _) k) <- zip'move DownRight z1
>   res <-  zip'upr z2 (A (Var "x") (Var "x")) >>= zip'all'the'way'up
>   liftIO $ (print "Result" >> print res)
>   --zip'through z1

*Zipper1> tz2
Next
Lf. Lx. f (Lf. f (Lf. Lx. x)) (f (Lf. Lx. x) x)
Next
Lx. f (Lf. f (Lf. Lx. x)) (f (Lf. Lx. x) x)
DownRight
f (Lf. f (Lf. Lx. x)) (f (Lf. Lx. x) x)
f (Lf. Lx. x) x
"replacing with"
x x
"Result"
Lf. Lx. f (Lf. f (Lf. Lx. x)) (x x)

We obtain an arbitrary number of cursors over the same data structure:
for example, in `tz2', the cursor 'z1' is still valid at the end, and
still points out to the `third' subterm. The cursor 'z2' is valid as
well. All these cursors are isolated: the updates done with the cursor
'z2' are invisible to the cursor 'z1' (as we can see if we uncomment
the last statement in tz2). Essentially, each cursor runs in its own
transaction. In the second part we will discuss how to make cursors
that see the updates of each other. Contrary to the traditional
database wisdom, for zippers, it is far harder to implement lower
isolation modes than the higher ones.


[Addendum]
We now implement a different convention for traversal steps:

> data Direction1 = FirstKid | RightKid | Parent deriving (Eq, Show)

Now it takes one step, Parent, to reach a parent of a subterm. We also
clarified the names of the steps to reach the first child or the last
child of a composite term.


> traverse1 :: (Monad m) => 
>              (Term -> m (Maybe Term, Direction1)) -> Term -> m Term
> traverse1 tf term = loop term
>  where 
>  loop term = do
> 	(term', direction) <- tf term
> 	let new_term = maybe term id term'
> 	let select Parent t = return t
> 	    select dir t@(L v t1) | dir == FirstKid || dir == RightKid = do
> 	       t1' <- loop t1
> 	       loop $ L v t1'
> 	    select RightKid t@(A t1 t2) = do
> 	       t2' <- loop t2
> 	       loop $ A t1 t2'
> 	    select FirstKid t@(A t1 t2) = do
> 	       t1' <- loop t1
> 	       loop $ A t1' t2
> 	select direction new_term

The zipper functions remain exactly as they are. The following example
illustrates the full traversal of a sample term
    (L "x" (A (Var "a") (Var "b")))

> tz3 :: IO () 
> tz3 = runCC $ do
>   z <- zip'term traverse1 (L "x" (A (Var "a") (Var "b")))
>   z <- zip'move FirstKid z
>   z <- zip'move FirstKid z
>   z <- zip'move Parent z
>   z <- zip'move RightKid z
>   z <- zip'move Parent z
>   z <- zip'move Parent z
>   ZipDone term <- zip'move Parent z
>   liftIO $ print "Done:"
>   liftIO $ print term

FirstKid
Lx. a b
FirstKid
a b
Parent
a
RightKid
a b
Parent
b
Parent
a b
Parent
Lx. a b
"Done:"
Lx. a b

The new traverse1 may be more intuitive but less convenient: there are
no Next step.

