{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Declaration where

import Language.Haskell.Exts.Annotated (Extension)
import Language.Haskell.Names (Symbol)
import Language.Haskell.Names.Interfaces ()
import Language.Haskell.Exts (ModuleName(ModuleName),prettyPrint)

import Data.Aeson (
    ToJSON(toJSON),object,(.=),encode,
    FromJSON(parseJSON),withObject,(.:),decode)
import qualified Data.ByteString.Lazy as ByteString (readFile,writeFile)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)

import Data.Maybe (fromMaybe)

data Declaration = Declaration Genre [Extension] DeclarationAST DeclaredSymbols MentionedSymbols
    deriving (Show,Eq,Ord)

data Genre =
    Value |
    TypeSignature |
    Type |
    TypeClass |
    ClassInstance |
    InfixFixity |
    DerivingInstance |
    FamilyInstance |
    ForeignImport |
    Other
        deriving (Show,Eq,Ord,Read)

type DeclarationAST   = Text
type DeclaredSymbols  = [Symbol]
type MentionedSymbols = [(Symbol,Maybe ModuleName)]

readDeclarations :: FilePath -> IO [Declaration]
readDeclarations declarationspath = do
    declarationsfile <- ByteString.readFile declarationspath
    return (fromMaybe (error "Failed to parse declarations") (decode declarationsfile))

writeDeclarations :: FilePath -> [Declaration] -> IO ()
writeDeclarations declarationspath declarations = do
    createDirectoryIfMissing True (dropFileName declarationspath)
    ByteString.writeFile declarationspath (encode declarations)

instance ToJSON Declaration where
    toJSON (Declaration declarationgenre extensions declarationast declaredsymbols mentionedsymbols) = object [
        "declarationgenre" .= show declarationgenre,
        "declarationextensions" .= map show extensions,
        "declarationast" .= declarationast,
        "declaredsymbols" .= declaredsymbols,
        "mentionedsymbols" .= fmap (fmap (fmap prettyPrint)) mentionedsymbols]

instance FromJSON Declaration where
    parseJSON = withObject "declaration object" (\o -> do
        declarationgenre <- fmap read (o .: "declarationgenre")
        declarationextensions <- fmap (map read) (o .: "declarationextensions")
        declarationast <- o .: "declarationast"
        declaredsymbols <- o .: "declaredsymbols"
        mentionedsymbols <- fmap (fmap (fmap (fmap ModuleName))) (o .: "mentionedsymbols")
        return (Declaration declarationgenre declarationextensions declarationast declaredsymbols mentionedsymbols))

