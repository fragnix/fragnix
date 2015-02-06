{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "GHC/Profiling.hs" #-}
-- | /Since: 4.7.0.0/
module GHC.Profiling where

foreign import ccall startProfTimer :: IO ()
foreign import ccall stopProfTimer :: IO ()
