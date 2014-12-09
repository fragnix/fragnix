module Main where

import Data.Tree (drawTree,Tree(Node))

tree :: Tree String
tree = Node "5" [Node "1" [Node "1" []],Node "2" []]

main :: IO ()
main = putStrLn (drawTree tree)

