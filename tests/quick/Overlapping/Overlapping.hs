{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Overlapping where

class Prim a where
    sizeOf :: a -> Int

class Vector v a where
    length :: v a -> Int

instance Prim Bool where
    sizeOf x = 2

instance Prim a => Vector [] a where
    length x = sum (map sizeOf x)

instance Vector [] Bool where
    length x = 1

