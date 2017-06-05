{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/OrdPSQ.hs" #-}
-- | An 'OrdPSQ' uses the 'Ord' instance of the key type to build a priority
-- search queue.
--
-- It is based on Ralf Hinze's work.
--
-- * Hinze, R., A Simple Implementation Technique for Priority Search Queues,
--   ICFP 2001, pp. 110-121
--
-- <http://citeseer.ist.psu.edu/hinze01simple.html>
--
-- This means it is similar to the
-- <http://hackage.haskell.org/package/PSQueue-1.1 PSQueue> package but
-- our benchmarks showed it perform quite a bit faster.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE BangPatterns        #-}
module Data.OrdPSQ
    ( -- * Type
      OrdPSQ

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

      -- * Delete/Update
    , delete
    , deleteMin
    , alter
    , alterMin

      -- * Conversion
    , fromList
    , toList
    , toAscList
    , keys

      -- * Views
    , insertView
    , deleteView
    , minView

      -- * Traversals
    , map
    , fold'

      -- * Validity check
    , valid
    ) where

import           Prelude hiding (map, lookup, null, foldr)

import           Data.OrdPSQ.Internal
