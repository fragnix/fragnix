{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Declaration where

import Language.Haskell.Names (Symbols)
import Language.Haskell.Names.Interfaces ()

import Data.Aeson (
    ToJSON(toJSON),object,(.=),encode,
    FromJSON(parseJSON),withObject,(.:),decode)
import qualified Data.ByteString.Lazy as ByteString (readFile,writeFile)
import Data.Text (Text)

import Data.Maybe (fromMaybe)

data Declaration = Declaration Genre DeclarationAST DeclaredSymbols MentionedSymbols
    deriving (Show,Eq)

data Genre = Value | TypeSignature [Text] | Type | TypeClass | ClassInstance | Other
    deriving (Show,Eq,Read)
type DeclarationAST   = Text
type DeclaredSymbols  = Symbols
type MentionedSymbols = Symbols

readDeclarations :: FilePath -> IO [Declaration]
readDeclarations declarationspath = do
    declarationsfile <- ByteString.readFile declarationspath
    return (fromMaybe (error "Failed to parse declarations") (decode declarationsfile))

writeDeclarations :: FilePath -> [Declaration] -> IO ()
writeDeclarations declarationspath declarations =
    ByteString.writeFile declarationspath (encode declarations)

instance ToJSON Declaration where
    toJSON (Declaration declarationgenre declarationast declaredsymbols mentionedsymbols) = object [
        "declarationgenre" .= show declarationgenre,
        "declarationast" .= declarationast,
        "declaredsymbols" .= declaredsymbols,
        "mentionedsymbols" .= mentionedsymbols]

instance FromJSON Declaration where
    parseJSON = withObject "declaration object" (\o -> do
        declarationgenre <- fmap read (o .: "declarationgenre")
        declarationast <- o .: "declarationast"
        declaredsymbols <- o .: "declaredsymbols"
        mentionedsymbols <- o .: "mentionedsymbols"
        return (Declaration declarationgenre declarationast declaredsymbols mentionedsymbols))

