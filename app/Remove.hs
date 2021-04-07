module Remove where

import Fragnix.Slice (readSlice, loadSliceIDsTransitive, deleteSlice)
import Fragnix.Paths (slicesPath)
import Utils (IDType(..), WithDeps(..))

import Control.Monad (forM_, unless)
import Data.Text (Text, pack, unpack)
import System.Directory (removeFile)

remove :: IDType -> WithDeps -> IO ()
remove (SliceID id) = removeSlice id
remove (EnvID id) = removeEnv id

removeSlice :: Text -> WithDeps -> IO ()
removeSlice id nodeps = do
  slice <- readSlice slicesPath id
  case nodeps of
    WithDeps -> do
      slicesToDelete <- loadSliceIDsTransitive slicesPath [id]
      forM_ slicesToDelete (deleteSlice slicesPath)
    WithoutDeps -> deleteSlice slicesPath id
      

removeEnv :: Text -> WithDeps -> IO ()
removeEnv id nodeps = do
  putStrLn $ unpack id