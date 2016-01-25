module SameTypeDifferentInstance1 where

data SameTypeDifferentInstance = SameTypeDifferentInstance

instance Eq SameTypeDifferentInstance where
    (==) SameTypeDifferentInstance SameTypeDifferentInstance = True
