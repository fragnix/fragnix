{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, OverloadedStrings,
             StandaloneDeriving #-}
module Fragnix.Loaf where

import Prelude hiding (readFile, writeFile)

import Fragnix.Core.Loaf (Loaf (..), LoafID)
import Fragnix.Core.Slice (SliceID)
import Fragnix.Paths (environmentPath)
import Fragnix.SliceSymbols (slicesToSymbols)
import Fragnix.Utils (listFilesRecursive)

import Control.Exception (Exception, throwIO)
import Control.Monad (forM)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.Hashable as Hashable (hash)
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Language.Haskell.Names (Symbol)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data LoafParseError = LoafParseError FilePath String

deriving instance Typeable LoafParseError
deriving instance Show LoafParseError

instance Exception LoafParseError

loafToSymbols :: Loaf -> IO [Symbol]
loafToSymbols Loaf { slices } = slicesToSymbols slices

symbolsToLoaf :: [Symbol] -> Text -> Loaf
symbolsToLoaf symbols loafID = Loaf {loafID, slices = symbolsToSliceIDs symbols}

symbolsToSliceIDs :: [Symbol] -> [SliceID]
symbolsToSliceIDs = error "not implemented"

pickLoaf :: [LoafID] -> IO Loaf
pickLoaf = readLoaf environmentPath . head

-- | Read the loaf with the given ID from the given directory
readLoaf :: FilePath -> LoafID -> IO Loaf
readLoaf loavesPath loafId = readLoafFile $ loavesPath </> unpack loafId

-- | Write the given slice to the given directory
writeLoaf :: FilePath -> Loaf -> IO ()
writeLoaf loavesPath loaf@Loaf { loafID } = do
  let loafPath = loavesPath </> unpack loafID
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

hashLoaf :: Loaf -> LoafID
hashLoaf a = pack (show (abs (fromIntegral (Hashable.hash a) :: Integer)))
