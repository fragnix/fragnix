module Greet where

putHello :: String -> IO ()
putHello x = putStrLn ("Hello " ++ x)

putHi :: String -> IO ()
putHi x = putStrLn ("Hi " ++ x)
