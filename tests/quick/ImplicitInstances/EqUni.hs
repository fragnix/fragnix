module EqUni where

import Uni (Uni(Uni))

instance Eq Uni where
    Uni == Uni = True