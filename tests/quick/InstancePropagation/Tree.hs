{-# LANGUAGE  DeriveDataTypeable #-}
module Tree where

import Data.Data (Data)

import {-# SOURCE #-} TypeableTree ()

data Tree a = Node{rootLabel :: a, subForest :: Forest a}
            deriving (Eq, Read, Show, Data)

type Forest a = [Tree a]