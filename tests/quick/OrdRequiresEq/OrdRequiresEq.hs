module OrdRequiresEq where

data Uni = Uni
    deriving (Ord)

instance Eq Uni where
    (==) Uni Uni = True
