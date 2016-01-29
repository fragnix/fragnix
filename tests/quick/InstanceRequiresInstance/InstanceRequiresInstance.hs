module InstanceRequiresInstance where

value :: T2 T1
value = c1

data T1 = T1

data T2 a = T2 a

class C1 a where
    c1 :: a

class C2 a where
    c2 :: a

instance C2 T1 where
    c2 = T1

instance (C2 a) => C1 (T2 a) where
    c1 = T2 c2

