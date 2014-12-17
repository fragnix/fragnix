module DataType where

import {-# SOURCE #-} Instance ()

data Digit a = One a
             | Two a a
             | Three a a a
             | Four a a a a