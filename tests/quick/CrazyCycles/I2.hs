module I2 where

import C1
import C2
import T

instance (C1 a) => C2 (T a) where
    c2 = T c1
