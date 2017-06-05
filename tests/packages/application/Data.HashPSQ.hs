{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/HashPSQ.hs" #-}
-- | A 'HashPSQ' offers very similar performance to 'IntPSQ'. In case of
-- collisions, it uses an 'OrdPSQ' locally to solve those.
--
-- This means worst case complexity is usually given by /O(min(n,W), log n)/,
-- where /W/ is the number of bits in an 'Int'. This simplifies to /O(min(n,W))/
-- since /log n/ is always smaller than /W/ on current machines.
module Data.HashPSQ
    ( -- * Type
      HashPSQ

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

import           Prelude               hiding (foldr, lookup, map, null)

import           Data.HashPSQ.Internal
