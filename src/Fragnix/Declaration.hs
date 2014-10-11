module Fragnix.Declaration where

data Declaration = Declaration

readDeclarations :: FilePath -> IO [Declaration]
readDeclarations = undefined

writeDeclarations :: FilePath -> [Declaration] -> IO ()
writeDeclarations = undefined

