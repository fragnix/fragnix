module Main where

putHello :: String -> IO ()
putHello x = putStrLn ("Hello " ++ x)

main :: IO ()
main = putHello "Fragnix!"