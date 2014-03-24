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

import Control.Monad.IO.Class (MonadIO(..))
import Sound.OSC
  ( Bundle(..), DuplexOSC, SendOSC(..)
  , bundle, waitReply, time, immediately)
import Sound.SC3
import Sound.SC3.Lepton.Tree.Tree
import Sound.SC3.Lepton.Tree.Diff

-- | Send OSC message for constructing given @SCNode@.
-- New node will be added to tail of target id.
addNode :: (SendOSC m, MonadIO m)
        => NodeId -- ^ Traget node id to add
        -> SCNode -- ^ New node
        -> m ()
addNode tId tree = do
  t0 <- time
  sendOSC $ bundle (t0 + 0.1) (treeToNew tId tree)

-- | Get node with specifying node id.
getNode :: (DuplexOSC m)
        => NodeId     -- ^ Node id to get
        -> m SCNode
getNode n = do
  sendOSC $ g_queryTree [(n,True)]
  m <- waitReply "/g_queryTree.reply"
  return $ parseNode m

-- | Send OSC message for setting given @SCNode@.
setNode :: (SendOSC m)
        => SCNode -- ^ Node with new parameters
        -> m ()
setNode t = sendOSC $ Bundle immediately (treeToSet t)

-- | Free the nodes specified with given node ids.
delNode :: (SendOSC m)
        => [NodeId]      -- ^ Node ids to free
        -> m ()
delNode ns = sendOSC $ n_free ns

-- | Experimental.
--
-- Modify running node with given function.
--
-- Implemented with freeing all nodes and adding transformed new nodes.
--
modifyNode :: (DuplexOSC m)
           => (SCNode -> SCNode) -- ^ Function to apply
           -> m ()
modifyNode f = do
  t <- getRootNode
  reset
  sendOSC $ Bundle immediately (treeToNew 0 (f t))

-- | Prints current SCNode with specifying node id.
printNode :: (MonadIO m, DuplexOSC m) => Int -> m ()
printNode n = getNode n >>= liftIO . putStrLn . renderNode True


--
-- Variants for root node
--

-- | Get root node.
getRootNode :: DuplexOSC m => m SCNode
getRootNode = getNode 0

-- | Print current SCNode entirely.
printRootNode :: (MonadIO m, DuplexOSC m) => m ()
printRootNode = printNode 0

-- | Patch node to same node of root node found in new node.
patchNode :: (DuplexOSC m, MonadIO m) => SCNode -> m ()
patchNode n = patchNodeTo (nodeId n) n

patchNodeTo :: (DuplexOSC m, MonadIO m) => Int -> SCNode -> m ()
patchNodeTo i t1 = do
  t0 <- getNode i
  let msgs = diffMessage t0 t1
  now <- time
  sendOSC $ bundle now msgs

getDiff :: DuplexOSC m => Int -> SCNode -> m SCNDiff
getDiff i t1 = return . flip diffSCN t1 =<< getNode i

-- | Update root node and then dump the contents.
patchPrint :: (DuplexOSC m, MonadIO m) => SCNode -> m ()
patchPrint n = patchNode n >>* printRootNode
