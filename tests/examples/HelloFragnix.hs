module Main where

putHello :: String -> IO ()
putHello x = putStrLn ("Hi " ++ x)

main :: IO ()
main = putHello "Fragnix!"