module BuiltinInstances where

class Binnary a where
  binn :: a -> a

instance (Binnary a, Binnary b) => Binnary (a, b) where
  binn (x, y) = (binn x, binn y)

instance Binnary Int where
  binn x = x

instance Binnary Bool where
  binn x = x

binfun :: (Int, Bool) -> (Int, Bool)
binfun = binn

