module OrdUni where

import Uni (Uni(Uni))

instance Ord Uni where
    compare Uni Uni = EQ
