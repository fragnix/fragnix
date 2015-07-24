{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module FamilyInstances where

data A = A
data B = B

type family State a

type instance State A = Int
type instance State B = Bool

f :: State B -> Bool
f x = x

class C a where
    c :: a

instance C Int where
    c = 0

class C (State a) => D a where
    d :: a

instance D A where
    d = A
