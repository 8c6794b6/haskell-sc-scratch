{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Network connection related functions and actions.

-}
module Sound.SC3.Tree.Connection
  ( addNode
  , getNode
  , getNode'
  , getDiff
  , setNode
  , delNode
  , printNode
  , printNode'
  , getRootNode
  , printRootNode
  , printRootNode'
  , patchNode
  , patchNodeTo
  , patchPrint
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Sound.OSC
  ( Bundle(..), DuplexOSC, SendOSC(..), Transport
  , bundle, waitReply, immediately)
import Sound.SC3 (g_queryTree, n_free)

import Sound.SC3.Tree.Type
import Sound.SC3.Tree.Diff

-- | Send OSC message for constructing given @SCNode@.
-- New node will be added to tail of target id.
addNode :: (SendOSC m, MonadIO m)
        => NodeId -- ^ Traget node id to add
        -> SCNode -- ^ New node
        -> m ()
addNode tId tree = do
  sendOSC $ bundle immediately (treeToNew tId tree)

-- | Get node with specifying node id.
getNode :: (DuplexOSC m)
        => NodeId     -- ^ Node id to get
        -> m SCNode
getNode n = do
  sendOSC $ g_queryTree [(n,True)]
  m <- waitReply "/g_queryTree.reply"
  return $ parseNode m

getNode' :: DuplexOSC m => NodeId -> m SCNode
getNode' n = do
  sendOSC $ g_queryTree [(n,False)]
  return . parseNode =<< waitReply "/g_queryTree.reply"

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

-- | Prints current SCNode with specifying node id.
printNode :: (MonadIO m, DuplexOSC m) => Int -> m ()
printNode n = getNode n >>= liftIO . putStrLn . drawSCNode

-- | Like 'printNode', but without parameters.
printNode' :: Transport m => Int -> m ()
printNode' n = getNode' n >>= liftIO . putStrLn . drawSCNode

-- | Patch node to same node of root node found in new node.
patchNode :: Transport m => SCNode -> m ()
patchNode n = patchNodeTo (nodeId n) n

-- | Patch node to specified node.
patchNodeTo :: Transport m => Int -> SCNode -> m ()
patchNodeTo i t1 = do
  t0 <- getNode i
  let msgs = diffMessage t0 t1
  sendOSC $ bundle immediately msgs

-- | Get difference of nodes.
getDiff :: DuplexOSC m => Int -> SCNode -> m SCNDiff
getDiff i t1 = return . flip diffSCNode t1 =<< getNode i

-- | Update root node and then dump the contents.
patchPrint :: Transport m => SCNode -> m ()
patchPrint n = patchNode n >>* printRootNode

--
-- Variants for root node
--

-- | Get root node.
getRootNode :: DuplexOSC m => m SCNode
getRootNode = getNode 0

-- | Print current SCNode entirely.
printRootNode :: Transport m => m ()
printRootNode = printNode 0

-- | Like 'printRootNode', but without parameters.
printRootNode' :: Transport m => m ()
printRootNode' = printNode' 0
