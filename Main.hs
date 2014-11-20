module Main where

import Fav (color)

import Data.Tree (drawTree,Tree(Node))

{-# INLINE putHello #-}

data A = A

instance Show A where
    show A = "this is" ++ " A"


tree :: Tree String
tree = Node "5" [Node "1" [Node "1" []],Node "2" []]

putHello :: String -> IO ()
putHello x = putStrLn ("HELLO " ++ x)

main :: IO ()
main = putStrLn (drawTree tree)

