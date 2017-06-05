{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "System/Log/FastLogger/IORef.hs" #-}









































































{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module System.Log.FastLogger.IORef (
       IORef
     , newIORef
     , readIORef
     , atomicModifyIORef'
     , writeIORef
     ) where

import Data.IORef

