module Main where

import Data.Tree (Tree(Node), drawTree)

tree :: Tree Integer
tree = Node 5 [Node 1 [], Node 2 []]

main :: IO ()
main = do
  putStrLn (drawTree (fmap show tree))

