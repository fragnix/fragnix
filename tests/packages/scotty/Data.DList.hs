{-# LINE 1 "Data/DList.hs" #-}
# 1 "Data/DList.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1






















































































# 8 "<command-line>" 2
# 1 "Data/DList.hs"
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-- For the IsList instance:
{-# LANGUAGE TypeFamilies #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList
-- Copyright   :  (c) Don Stewart 2006-2009, (c) Sean Leather 2013
-- License     :  See LICENSE file
--
-- Maintainer  :  sean.leather@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- Difference lists: a data structure for /O(1)/ append on lists.
--
-----------------------------------------------------------------------------

module Data.DList (

   DList

  -- * Construction
  ,fromList      -- :: [a] -> DList a
  ,toList        -- :: DList a -> [a]
  ,apply         -- :: DList a -> [a] -> [a]

  -- * Basic functions
  ,empty         -- :: DList a
  ,singleton     -- :: a -> DList a
  ,cons          -- :: a -> DList a -> DList a
  ,snoc          -- :: DList a -> a -> DList a
  ,append        -- :: DList a -> DList a -> DList a
  ,concat        -- :: [DList a] -> DList a
  ,replicate     -- :: Int -> a -> DList a
  ,list          -- :: b -> (a -> DList a -> b) -> DList a -> b
  ,head          -- :: DList a -> a
  ,tail          -- :: DList a -> DList a
  ,unfoldr       -- :: (b -> Maybe (a, b)) -> b -> DList a
  ,foldr         -- :: (a -> b -> b) -> b -> DList a -> b
  ,map           -- :: (a -> b) -> DList a -> DList b

  ) where

import Prelude hiding (concat, foldr, map, head, tail, replicate)
import qualified Data.List as List
import Control.DeepSeq (NFData(..))
import Control.Monad as M
import Data.Monoid
import Data.Function (on)
import Data.String (IsString(..))

import Data.Foldable (Foldable)
import qualified Data.Foldable as F



import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec,
                  readListPrecDefault)


import GHC.Exts (IsList)
-- This is for the IsList methods, which conflict with fromList, toList:
import qualified GHC.Exts




import Control.Applicative(Applicative(..), Alternative, (<|>))
import qualified Control.Applicative (empty)

-- | A difference list is a function that, given a list, returns the original
-- contents of the difference list prepended to the given list.
--
-- This structure supports /O(1)/ append and snoc operations on lists, making it
-- very useful for append-heavy uses (esp. left-nested uses of 'List.++'), such
-- as logging and pretty printing.
--
-- Here is an example using DList as the state type when printing a tree with
-- the Writer monad:
--
-- > import Control.Monad.Writer
-- > import Data.DList
-- >
-- > data Tree a = Leaf a | Branch (Tree a) (Tree a)
-- >
-- > flatten_writer :: Tree x -> DList x
-- > flatten_writer = snd . runWriter . flatten
-- >     where
-- >       flatten (Leaf x)     = tell (singleton x)
-- >       flatten (Branch x y) = flatten x >> flatten y
--
newtype DList a = DL { unDL :: [a] -> [a] }

-- | Convert a list to a dlist
fromList    :: [a] -> DList a
fromList    = DL . (++)
{-# INLINE fromList #-}

-- | Convert a dlist to a list
toList      :: DList a -> [a]
toList      = ($[]) . unDL
{-# INLINE toList #-}

-- | Apply a dlist to a list to get the underlying list with an extension
--
-- > apply (fromList xs) ys = xs ++ ys
apply       :: DList a -> [a] -> [a]
apply       = unDL

-- | Create a dlist containing no elements
empty       :: DList a
empty       = DL id
{-# INLINE empty #-}

-- | Create dlist with a single element
singleton   :: a -> DList a
singleton   = DL . (:)
{-# INLINE singleton #-}

-- | /O(1)/. Prepend a single element to a dlist
infixr `cons`
cons        :: a -> DList a -> DList a
cons x xs   = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- | /O(1)/. Append a single element to a dlist
infixl `snoc`
snoc        :: DList a -> a -> DList a
snoc xs x   = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- | /O(1)/. Append dlists
append       :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

-- | /O(spine)/. Concatenate dlists
concat       :: [DList a] -> DList a
concat       = List.foldr append empty
{-# INLINE concat #-}

-- | /O(n)/. Create a dlist of the given number of elements
replicate :: Int -> a -> DList a
replicate n x = DL $ \xs -> let go m | m <= 0    = xs
                                     | otherwise = x : go (m-1)
                            in go n
{-# INLINE replicate #-}

-- | /O(n)/. List elimination for dlists
list :: b -> (a -> DList a -> b) -> DList a -> b
list nill consit dl =
  case toList dl of
    [] -> nill
    (x : xs) -> consit x (fromList xs)

-- | /O(n)/. Return the head of the dlist
head :: DList a -> a
head = list (error "Data.DList.head: empty dlist") const

-- | /O(n)/. Return the tail of the dlist
tail :: DList a -> DList a
tail = list (error "Data.DList.tail: empty dlist") (flip const)

-- | /O(n)/. Unfoldr for dlists
unfoldr :: (b -> Maybe (a, b)) -> b -> DList a
unfoldr pf b =
  case pf b of
    Nothing     -> empty
    Just (a, b') -> cons a (unfoldr pf b')

-- | /O(n)/. Foldr over difference lists
foldr        :: (a -> b -> b) -> b -> DList a -> b
foldr f b    = List.foldr f b . toList
{-# INLINE foldr #-}

-- | /O(n)/. Map over difference lists.
map          :: (a -> b) -> DList a -> DList b
map f        = foldr (cons . f) empty
{-# INLINE map #-}

instance Eq a => Eq (DList a) where
    (==) = (==) `on` toList

instance Ord a => Ord (DList a) where
    compare = compare `on` toList

-- The Read and Show instances were adapted from Data.Sequence.

instance Read a => Read (DList a) where

  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    dl <- readPrec
    return (fromList dl)
  readListPrec = readListPrecDefault







instance Show a => Show (DList a) where
  showsPrec p dl = showParen (p > 10) $
    showString "fromList " . shows (toList dl)

instance Monoid (DList a) where
    mempty  = empty
    mappend = append

instance Functor DList where
    fmap = map
    {-# INLINE fmap #-}

instance Applicative DList where
    pure  = return
    (<*>) = ap

instance Alternative DList where
    empty = empty
    (<|>) = append

instance Monad DList where
  m >>= k
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = List.foldr append empty . List.map k . toList $ m
    -- = List.foldr (append . k) empty . toList $ m
    = foldr (append . k) empty m
  {-# INLINE (>>=) #-}

  return x = singleton x
  {-# INLINE return #-}

  fail _   = empty
  {-# INLINE fail #-}

instance MonadPlus DList where
  mzero    = empty
  mplus    = append

instance Foldable DList where
  fold        = mconcat . toList
  {-# INLINE fold #-}

  foldMap f   = F.foldMap f . toList
  {-# INLINE foldMap #-}

  foldr f x   = List.foldr f x . toList
  {-# INLINE foldr #-}

  foldl f x   = List.foldl f x . toList
  {-# INLINE foldl #-}

  foldr1 f    = List.foldr1 f . toList
  {-# INLINE foldr1 #-}

  foldl1 f    = List.foldl1 f . toList
  {-# INLINE foldl1 #-}

-- CPP: foldl', foldr' added to Foldable in 7.6.1
-- http://www.haskell.org/ghc/docs/7.6.1/html/users_guide/release-7-6-1.html

  foldl' f x  = List.foldl' f x . toList
  {-# INLINE foldl' #-}

  foldr' f x  = F.foldr' f x . toList
  {-# INLINE foldr' #-}


instance NFData a => NFData (DList a) where
  rnf = rnf . toList
  {-# INLINE rnf #-}

instance IsString (DList Char) where
  fromString = fromList
  {-# INLINE fromString #-}


instance IsList (DList a) where
  type Item (DList a) = a
  fromList = fromList
  {-# INLINE fromList #-}
  toList = toList
  {-# INLINE toList #-}


