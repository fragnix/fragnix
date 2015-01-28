{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/Wai/Handler/Warp/Windows.hs" #-}













































































{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Warp.Windows
  ( windowsThreadBlockHack
  ) where

windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack = id
