module Main where

import Fav (color)

{-# INLINE putHello #-}

data A = A

instance Show A where
    show A = "this is" ++ " A"


putHello :: String -> IO ()
putHello x = putStrLn ("HELLO " ++ x)

main :: IO ()
main = putHello (show A)

