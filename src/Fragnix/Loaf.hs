{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, OverloadedStrings,
             StandaloneDeriving #-}
module Fragnix.Loaf where

import Prelude hiding (readFile, writeFile)

import Fragnix.Core.Loaf (Loaf (..), LoafID)
import qualified Fragnix.Core.Loaf as F (Name (..), Symbol (..))
import qualified Fragnix.Core.Slice as S
import Fragnix.Paths (environmentPath)
import Fragnix.Utils (listFilesRecursive)

import Control.Exception (Exception, throwIO)
import Control.Monad (filterM, forM, forM_)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Char (isDigit)
import qualified Data.Hashable as Hashable (hash)
import qualified Data.Map as Map (empty, foldrWithKey, insert)
import qualified Data.Text as T (Text, pack, unpack)
import Data.Typeable (Typeable)
import qualified Language.Haskell.Exts as E
    (ModuleName (..), Name (..), prettyPrint)
import qualified Language.Haskell.Names as N (Environment, Symbol (..))
import System.Directory
    (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

data LoafParseError = LoafParseError FilePath String

deriving instance Typeable LoafParseError
deriving instance Show LoafParseError

instance Exception LoafParseError

pickLoaf :: [LoafID] -> IO Loaf
pickLoaf = readLoaf environmentPath . head

-- | Read the loaf with the given ID from the given directory
readLoaf :: FilePath -> LoafID -> IO Loaf
readLoaf loavesPath loafId = readLoafFile $ loavesPath </> T.unpack loafId

readAllLoaves :: FilePath -> IO [Loaf]
readAllLoaves path = do
    createDirectoryIfMissing True path
    filenames <- getDirectoryContents path
    let pathloafnames = map (\filename -> (filename, path </> filename)) filenames
    existingPathLoafNames <- filterM (doesFileExist . snd) pathloafnames
    forM existingPathLoafNames (\(_,loafPath) -> do readLoafFile loafPath)


readLoaves :: FilePath -> [LoafID] -> IO [Loaf]
readLoaves path ids = do
  forM ids (readLoaf path)

-- | Write the given slice to the given directory
writeLoaf :: FilePath -> Loaf -> IO ()
writeLoaf loavesPath loaf@Loaf { loafID } = do
  let loafPath = loavesPath </> T.unpack loafID
  createDirectoryIfMissing True environmentPath
  writeFile loafPath (encodePretty loaf)

getLoaves :: FilePath -> IO [Loaf]
getLoaves path = do
  loafPaths <- listFilesRecursive path
  forM loafPaths readLoafFile

readLoafFile :: FilePath -> IO Loaf
readLoafFile path = do
  loafFile <- readFile path
  either (throwIO . LoafParseError path) return (eitherDecode loafFile)

hashLoaf :: (T.Text, [F.Symbol]) -> LoafID
hashLoaf a = T.pack (show (abs (fromIntegral (Hashable.hash a) :: Integer)))


persistEnvironment :: FilePath -> N.Environment -> IO ()
persistEnvironment path environment = do
    createDirectoryIfMissing True path
    forM_ (envToLoaves environment) (writeLoaf path)



-- Isomorphism between Names

coreNameToName :: F.Name -> E.Name ()
coreNameToName (F.Ident t)  = E.Ident () (T.unpack t)
coreNameToName (F.Symbol t) = E.Symbol () (T.unpack t)

nameToCoreName :: E.Name () -> F.Name
nameToCoreName (E.Ident () s)  = F.Ident (T.pack s)
nameToCoreName (E.Symbol () s) = F.Symbol (T.pack s)

-- Isomorphism between ModuleName and Reference

referenceToModuleName :: S.Reference -> E.ModuleName ()
referenceToModuleName (S.OtherSlice t) = E.ModuleName () ("F" <> T.unpack t)
referenceToModuleName (S.Builtin t)    = E.ModuleName () (T.unpack t)

moduleNameToReference :: E.ModuleName a -> S.Reference
moduleNameToReference (E.ModuleName _ ('F':rest)) | all isDigit rest = S.OtherSlice (T.pack rest)
moduleNameToReference (E.ModuleName _ moduleName) = S.Builtin (T.pack moduleName)

-- Isomorphism between Symbols

coreSymbolToSymbol :: F.Symbol -> N.Symbol
coreSymbolToSymbol (F.Value m n) =
  N.Value (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.Method m n1 n2) =
  N.Method (referenceToModuleName m) (coreNameToName n1) (coreNameToName n2)
coreSymbolToSymbol (F.Selector m n1 n2 ns) =
  N.Selector (referenceToModuleName m) (coreNameToName n1) (coreNameToName n2) (coreNameToName <$> ns)
coreSymbolToSymbol (F.Constructor m n1 n2) =
  N.Constructor (referenceToModuleName m) (coreNameToName n1) (coreNameToName n2)
coreSymbolToSymbol (F.Type m n) =
  N.Type (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.Data m n) =
  N.Data (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.NewType m n) =
  N.NewType (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.TypeFam m n mn) =
  N.TypeFam (referenceToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.DataFam m n mn) =
  N.DataFam (referenceToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.Class m n) =
  N.Class (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.PatternConstructor m n mn) =
  N.PatternConstructor (referenceToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.PatternSelector m n1 mn n2) =
  N.PatternSelector (referenceToModuleName m) (coreNameToName n1) (coreNameToName <$> mn) (coreNameToName n2)

symbolToCoreSymbol :: N.Symbol -> F.Symbol
symbolToCoreSymbol (N.Value m n) =
  F.Value (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.Method m n1 n2) =
  F.Method (moduleNameToReference m) (nameToCoreName n1) (nameToCoreName n2)
symbolToCoreSymbol (N.Selector m n1 n2 ns) =
  F.Selector (moduleNameToReference m) (nameToCoreName n1) (nameToCoreName n2) (nameToCoreName <$> ns)
symbolToCoreSymbol (N.Constructor m n1 n2) =
  F.Constructor (moduleNameToReference m) (nameToCoreName n1) (nameToCoreName n2)
symbolToCoreSymbol (N.Type m n) =
  F.Type (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.Data m n) =
  F.Data (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.NewType m n) =
  F.NewType (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.TypeFam m n mn) =
  F.TypeFam (moduleNameToReference m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.DataFam m n mn) =
  F.DataFam (moduleNameToReference m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.Class m n) =
  F.Class (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.PatternConstructor m n mn) =
  F.PatternConstructor (moduleNameToReference m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.PatternSelector m n1 mn n2) =
  F.PatternSelector (moduleNameToReference m) (nameToCoreName n1) (nameToCoreName <$> mn) (nameToCoreName n2)

-- Isomorphism between list of FragnixModules and Environment

envToLoaves :: N.Environment -> [Loaf]
envToLoaves = Map.foldrWithKey moduleToLoaf []

loavesToEnv :: [Loaf] -> N.Environment
loavesToEnv = foldr addLoafToEnv Map.empty

moduleToLoaf :: E.ModuleName () -> [N.Symbol] -> [Loaf] -> [Loaf]
moduleToLoaf modulname syms loafList = Loaf { name, symbols, loafID} : loafList
  where name = T.pack $ E.prettyPrint modulname
        symbols = fmap symbolToCoreSymbol syms
        loafID = hashLoaf (name, symbols)

addLoafToEnv :: Loaf -> N.Environment -> N.Environment
addLoafToEnv Loaf{name,symbols} = Map.insert (E.ModuleName () (T.unpack name)) (fmap coreSymbolToSymbol symbols)
