module Main where

import Fav (color)


putHello :: String -> IO ()
putHello x = putStrLn ("HELLO " ++ x)

main :: IO ()
main = do
    config <- readFile "config.xml"
    putHello config


f x = g x

g x = f x
