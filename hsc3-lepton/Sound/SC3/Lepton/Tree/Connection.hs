------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Network connection related functions and actions.
-- 
module Sound.SC3.Lepton.Tree.Connection 
  ( addNode
  , getNode
  , getDiff
  , setNode
  , delNode
  , modifyNode
  , printNode
  , getRootNode
  , printRootNode
  , patchNode
  , patchNodeTo
  , patchPrint  
  ) where

import Sound.OpenSoundControl 
  (Transport(..), OSC(..), Time(..), wait, utcr, immediately)
import Sound.SC3
import Sound.SC3.Lepton.Tree.Tree
import Sound.SC3.Lepton.Tree.Diff

import Sound.SC3.Lepton.Util (queryTree)

-- | Send OSC message for constructing given @SCNode@.
-- New node will be added to tail of target id.
addNode :: (Transport t)
        => NodeId -- ^ Traget node id to add
        -> SCNode -- ^ New node
        -> t      -- ^ Scsynth connection
        -> IO ()
addNode tId tree = \fd -> do
  t0 <- utcr
  send fd (Bundle (UTCr (t0 + 0.1)) (treeToNew tId tree))

-- | Get node with specifying node id.
getNode :: (Transport t)
        => NodeId     -- ^ Node id to get
        -> t          -- ^ Connection
        -> IO SCNode
getNode n fd = do
  send fd (queryTree n)
  m <- wait fd "/g_queryTree.reply"
  return $ parseNode m

-- | Send OSC message for setting given @SCNode@.
setNode :: (Transport t)
        => SCNode -- ^ Node with new parameters
        -> t
        -> IO ()
setNode t = \fd -> send fd $ Bundle immediately (treeToSet t)

-- | Free the nodes specified with given node ids.
delNode :: (Transport t)
        => [NodeId]      -- ^ Node ids to free
        -> t
        -> IO ()
delNode ns = \fd -> send fd $ n_free ns

-- | Experimental.
--
-- Modify running node with given function.
--
-- Implemented with freeing all nodes and adding transformed new nodes.
--
modifyNode :: (Transport t)
           => (SCNode -> SCNode) -- ^ Function to apply
           -> t
           -> IO ()
modifyNode f = \fd -> do
  t <- getRootNode fd
  reset fd
  send fd $ Bundle immediately (treeToNew 0 (f t))

-- | Prints current SCNode with specifying node id.
printNode :: Transport t => Int -> t -> IO ()
printNode n fd = getNode n fd >>= putStrLn . renderNode True

--
-- Variants for root node
--

-- | Get root node.
getRootNode :: (Transport t) => t -> IO SCNode
getRootNode = getNode 0

-- | Print current SCNode entirely.
printRootNode :: (Transport t) => t -> IO ()
printRootNode = printNode 0

-- | Patch node to same node of root node found in new node.
patchNode :: Transport t => SCNode -> t -> IO ()
patchNode n = patchNodeTo (nodeId n) n

patchNodeTo :: Transport t => Int -> SCNode -> t -> IO ()
patchNodeTo i t1 fd = do
  t0 <- getNode i fd
  let msgs = diffMessage t0 t1
  now <- utcr
  send fd $ Bundle (UTCr now) msgs

getDiff :: Transport t => Int -> SCNode -> t -> IO SCNDiff
getDiff i t1 fd = do
  t0 <- getNode i fd
  return $ diffSCN t0 t1

-- | Update root node and then dump the contents.
patchPrint :: Transport t => SCNode -> t -> IO ()
patchPrint n = patchNode n >>* printRootNode

