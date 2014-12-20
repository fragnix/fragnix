{-# LANGUAGE  DeriveDataTypeable #-}
module Tree where

import Data.Data (Data)
import Data.Typeable (Typeable)


data Tree a = Node{rootLabel :: a, subForest :: Forest a}
            deriving (Eq, Read, Show, Data, Typeable)

type Forest a = [Tree a]