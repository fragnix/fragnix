module Sized where

import {-# SOURCE #-} SizedDigit ()

class Sized a where
        size :: a -> Int
