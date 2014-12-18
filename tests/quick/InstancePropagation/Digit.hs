module Digit where

import {-# SOURCE #-} FoldableDigit ()
import {-# SOURCE #-} FunctorDigit ()

data Digit a = One a
             | Two a a
             | Three a a a
             | Four a a a a