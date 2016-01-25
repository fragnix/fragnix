module SameTypeDifferentInstance2 where

data SameTypeDifferentInstance = SameTypeDifferentInstance

instance Eq SameTypeDifferentInstance where
    (/=) SameTypeDifferentInstance SameTypeDifferentInstance = False
