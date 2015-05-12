{-# LANGUAGE DeriveDataTypeable,
  MultiParamTypeClasses, BangPatterns, ScopedTypeVariables,
  TypeFamilies, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module MVectorClass where

class MVector v a where
    basicLength :: v s a -> Int

