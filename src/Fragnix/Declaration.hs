{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Declaration where

import Language.Haskell.Names (Symbols)
import Language.Haskell.Names.Interfaces ()

import Data.Aeson (ToJSON(toJSON),object,(.=),encode)
import qualified Data.ByteString.Lazy as ByteString (writeFile)

data Declaration = Declaration Genre DeclarationAST DeclaredSymbols UsedSymbols
    deriving (Show,Eq)

data Genre = Value | TypeSignature | Type | TypeClass | ClassInstance | Other
    deriving (Show,Eq,Read)
type DeclarationAST  = String
type DeclaredSymbols = Symbols
type UsedSymbols     = Symbols

readDeclarations :: FilePath -> IO [Declaration]
readDeclarations = undefined

writeDeclarations :: FilePath -> [Declaration] -> IO ()
writeDeclarations declarationspath declarations =
    ByteString.writeFile declarationspath (encode declarations)

instance ToJSON Declaration where
    toJSON (Declaration genre declarationast declaredsymbols usedsymbols) = object [
        "declarationgenre" .= show genre,
        "declarationast" .= declarationast,
        "declaredsymbols" .= declaredsymbols,
        "mentionedsymbols" .= usedsymbols]

