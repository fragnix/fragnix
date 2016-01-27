module ClassInContext where

class FFF a where
    fff :: a -> a

data S a = S a

instance FFF Int where
    fff x = x

instance (Eq a, FFF a) => Eq (S a) where
    (S x) == (S y) = (fff x) == (fff y)

cmpr :: S Int -> S Int -> Bool
cmpr = (==)

