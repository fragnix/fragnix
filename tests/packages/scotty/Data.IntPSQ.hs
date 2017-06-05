{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/IntPSQ.hs" #-}























































-- | 'IntPSQ' fixes the key type to 'Int'. It is generally much faster than
-- an 'OrdPSQ'.
--
-- Many operations have a worst-case complexity of O(min(n,W)). This means that
-- the operation can -- become linear in the number of elements with a maximum
-- of W -- the number of bits in an Int (32 or 64).
{-# LANGUAGE CPP           #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.IntPSQ
    ( -- * Type
      IntPSQ

      -- * Query
    , null
    , size
    , member
    , lookup
    , findMin

      -- * Construction
    , empty
    , singleton

      -- * Insertion
    , insert

      -- * Delete/update
    , delete
    , deleteMin
    , alter
    , alterMin

      -- * Lists
    , fromList
    , toList
    , keys

      -- * Views
    , insertView
    , deleteView
    , minView

      -- * Traversal
    , map
    , fold'

      -- * Validity check
    , valid
    ) where

import           Prelude hiding (lookup, map, filter, foldr, foldl, null)

import           Data.IntPSQ.Internal
