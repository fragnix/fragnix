{-# LANGUAGE Haskell98, CPP #-}
{-# LINE 1 "src/Data/Vault/Strict.hs" #-}
























































-- | A persistent store for values of arbitrary types.
--
-- The 'Vault' type in this module is strict in both keys and values.
module Data.Vault.Strict (
    -- * Vault
    Vault, Key,
    empty, newKey, lookup, insert, adjust, delete, union,

    -- * Locker
    Locker,
    lock, unlock,
    ) where

import Prelude hiding (lookup)
import Control.Monad.ST
import qualified Data.Vault.ST.Strict as ST


{-----------------------------------------------------------------------------
    Vault
------------------------------------------------------------------------------}

-- | A persistent store for values of arbitrary types.
--
-- This variant is the simplest and creates keys in the 'IO' monad.
-- See the module "Data.Vault.ST" if you want to use it with the 'ST' monad instead.
type Vault = ST.Vault RealWorld

-- | Keys for the vault.
type Key = ST.Key RealWorld

-- | The empty vault.
empty :: Vault
empty = ST.empty

-- | Create a new key for use with a vault.
newKey :: IO (Key a)
newKey = stToIO ST.newKey

-- | Lookup the value of a key in the vault.
lookup :: Key a -> Vault -> Maybe a
lookup = ST.lookup

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key a -> a -> Vault -> Vault
insert = ST.insert

-- | Adjust the value for a given key if it's present in the vault.
adjust :: (a -> a) -> Key a -> Vault -> Vault
adjust = ST.adjust

-- | Delete a key from the vault.
delete :: Key a -> Vault -> Vault
delete = ST.delete

-- | Merge two vaults (left-biased).
union :: Vault -> Vault -> Vault
union = ST.union

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}

-- | A persistent store for a single value.
type Locker = ST.Locker RealWorld

-- | Put a single value into a 'Locker'.
lock :: Key a -> a -> Locker
lock = ST.lock

-- | Retrieve the value from the 'Locker'.
unlock :: Key a -> Locker -> Maybe a
unlock = ST.unlock
