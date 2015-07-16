{-# LANGUAGE TypeFamilies #-}
module FamilyInstances where

data A = A
data B = B

type family State a

type instance State A = Int
type instance State B = Bool

f :: State B -> Bool
f x = x
