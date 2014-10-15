module Main where

import Fav (color)

f x = g x

g x = f x

putHello :: String -> IO ()
putHello x = putStrLn ("HELLO " ++ x)

main :: IO ()
main = do
    config <- readFile "config.xml"
    putHello config
