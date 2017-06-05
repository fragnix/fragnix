{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HTTP2/Priority.hs" #-}



































































{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-- | This is partial implementation of the priority of HTTP/2.
--
-- This implementation does support structured priority queue
-- but not support re-structuring. This means that it is assumed that
-- an entry created by a Priority frame is never closed. The entry
-- behaves an intermediate node, not a leaf.
--
-- This queue is fair for weight. Consider two weights: 201 and 101.
-- Repeating enqueue/dequeue probably produces
-- 201, 201, 101, 201, 201, 101, ...
--
-- Only one entry per stream should be enqueued.

module Network.HTTP2.Priority (
  -- * Precedence
    Precedence
  , defaultPrecedence
  , toPrecedence
  -- * PriorityTree
  , PriorityTree
  , newPriorityTree
  -- * PriorityTree functions
  , prepare
  , enqueue
  , dequeue
  , dequeueSTM
  , isEmpty
  , isEmptySTM
  , delete
  ) where

import Control.Concurrent.STM
import Control.Monad (when, unless)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Network.HTTP2.Priority.Queue (TPriorityQueue, Precedence)
import qualified Network.HTTP2.Priority.Queue as Q
import Network.HTTP2.Types

----------------------------------------------------------------

-- | Abstract data type for priority trees.
data PriorityTree a = PriorityTree !(TVar (Glue a))
                                   !(TNestedPriorityQueue a)

type Glue a = IntMap (TNestedPriorityQueue a, Precedence)

-- INVARIANT: Empty TNestedPriorityQueue is never enqueued in
-- another TNestedPriorityQueue.
type TNestedPriorityQueue a = TPriorityQueue (Element a)

data Element a = Child !a
               | Parent !(TNestedPriorityQueue a)


----------------------------------------------------------------

-- | Default precedence.
defaultPrecedence :: Precedence
defaultPrecedence = toPrecedence defaultPriority

-- | Converting 'Priority' to 'Precedence'.
--   When an entry is enqueued at the first time,
--   this function should be used.
toPrecedence :: Priority -> Precedence
toPrecedence (Priority _ dep w) = Q.Precedence 0 w dep

----------------------------------------------------------------

-- | Creating a new priority tree.
newPriorityTree :: IO (PriorityTree a)
newPriorityTree = PriorityTree <$> newTVarIO Map.empty
                               <*> atomically Q.new

----------------------------------------------------------------

-- | Bringing up the structure of the priority tree.
--   This must be used for Priority frame.
prepare :: PriorityTree a -> StreamId -> Priority -> IO ()
prepare (PriorityTree var _) sid p = atomically $ do
    q <- Q.new
    let pre = toPrecedence p
    modifyTVar' var $ Map.insert sid (q, pre)

-- | Enqueuing an entry to the priority tree.
--   This must be used for Header frame.
enqueue :: PriorityTree a -> StreamId -> Precedence -> a -> IO ()
enqueue (PriorityTree var q0) sid p0 x = atomically $ do
    m <- readTVar var
    let !el = Child x
    loop m el p0
  where
    loop m el p
      | pid == 0  = Q.enqueue q0 sid p el
      | otherwise = case Map.lookup pid m of
          -- If not found, enqueuing it to the stream 0 queue.
          Nothing -> Q.enqueue q0 sid p el
          Just (q', p') -> do
              notQueued <- Q.isEmpty q'
              Q.enqueue q' sid p el
              when notQueued $ do
                  let !el' = Parent q'
                  loop m el' p'
      where
        pid = Q.dependency p


-- | Checking if the priority tree is empty.
isEmpty :: PriorityTree a -> IO Bool
isEmpty = atomically . isEmptySTM

-- | Checking if the priority tree is empty.
isEmptySTM :: PriorityTree a -> STM Bool
isEmptySTM (PriorityTree _ q0) = Q.isEmpty q0

-- | Dequeuing an entry from the priority tree.
dequeue :: PriorityTree a -> IO (StreamId, Precedence, a)
dequeue = atomically . dequeueSTM

-- | Dequeuing an entry from the priority tree.
dequeueSTM :: PriorityTree a -> STM (StreamId, Precedence, a)
dequeueSTM (PriorityTree _ q0) = loop q0
  where
    loop q = do
        (sid,p,el) <- Q.dequeue q
        case el of
            Child x   -> return $! (sid, p, x)
            Parent q' -> do
                entr <- loop q'
                empty <- Q.isEmpty q'
                unless empty $ Q.enqueue q sid p el
                return entr

-- | Deleting the entry corresponding to 'StreamId'.
--   'delete' and 'enqueue' are used to change the priority of
--   a live stream.
delete :: PriorityTree a -> StreamId -> Precedence -> IO (Maybe a)
delete (PriorityTree var q0) sid p
  | pid == 0  = atomically $ del q0
  | otherwise = atomically $ do
        m <- readTVar var
        case Map.lookup pid m of
            Nothing    -> return Nothing
            Just (q,_) -> del q
  where
    pid = Q.dependency p
    del q = do
        mel <- Q.delete sid q
        case mel of
            Nothing -> return Nothing
            Just el -> case el of
                Child  x -> return $ Just x
                Parent _ -> return Nothing -- fixme: this is error
