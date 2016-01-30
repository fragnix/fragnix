module Value where

import qualified SameTypeDifferentInstance1
import qualified SameTypeDifferentInstance2

value :: String
value = show SameTypeDifferentInstance1.SameTypeDifferentInstance ++
        show SameTypeDifferentInstance2.SameTypeDifferentInstance

