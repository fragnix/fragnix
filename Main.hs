module Main where

import Fav (color)

{-# INLINE putHello #-}

data A = A

instance Show A where
    show A = "this is" ++ " A"


putHello :: String -> IO ()
putHello x = putStrLn ("HELLO " ++ x)

main :: IO ()
main = do
    config <- readFile "config.xml"
    putHello config


f x = g x

g x = f x
