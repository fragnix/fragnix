--{-# LANGUAGE TypeFamilies #-}
module ExistentialFamilies where

import Control.Monad.ST (runST,ST)

un :: ()
un = runST f where
    f = return un :: ST s ()
