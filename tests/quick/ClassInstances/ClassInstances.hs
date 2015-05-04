module ClassInstances where

data Uni = Uni

instance Eq Uni where
    Uni == Uni = True

instance Ord Uni where
    compare Uni Uni = EQ
