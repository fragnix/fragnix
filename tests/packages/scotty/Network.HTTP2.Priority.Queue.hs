{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HTTP2/Priority/Queue.hs" #-}



































































{-# LANGUAGE CPP #-}

module Network.HTTP2.Priority.Queue (
    Precedence(..)
  , TPriorityQueue
  , new
  , isEmpty
  , enqueue
  , dequeue
  , delete
  ) where

import Control.Concurrent.STM
import Network.HTTP2.Priority.PSQ (PriorityQueue, Key, Precedence(..))
import qualified Network.HTTP2.Priority.PSQ as Q

----------------------------------------------------------------

newtype TPriorityQueue a = TPriorityQueue (TVar (PriorityQueue a))

new :: STM (TPriorityQueue a)
new = TPriorityQueue <$> newTVar Q.empty

isEmpty :: TPriorityQueue a -> STM Bool
isEmpty (TPriorityQueue th) = Q.isEmpty <$> readTVar th

enqueue :: TPriorityQueue a -> Key -> Precedence -> a -> STM ()
enqueue (TPriorityQueue th) k p v = modifyTVar' th $ Q.enqueue k p v

dequeue :: TPriorityQueue a -> STM (Key, Precedence, a)
dequeue (TPriorityQueue th) = do
  h <- readTVar th
  case Q.dequeue h of
    Nothing -> retry
    Just (k, p, v, h') -> do
      writeTVar th h'
      return (k, p, v)

delete :: Key -> TPriorityQueue a -> STM (Maybe a)
delete k (TPriorityQueue th) = do
    q <- readTVar th
    let (mv, q') = Q.delete k q
    writeTVar th q'
    return mv
