module Remove where

import Fragnix.Slice (readSlice, loadSliceIDsTransitive, deleteSlice)
import Fragnix.Paths (slicesPath)
import Utils (IDType(..))

import Control.Monad (forM_, unless)
import Data.Text (pack)
import System.Directory (removeFile)

remove :: IDType -> String -> Bool -> IO ()
remove SliceID = removeSlice
remove EnvID = removeEnv

removeSlice :: String -> Bool -> IO ()
removeSlice id nodeps = do
  slice <- readSlice slicesPath $ pack id
  if nodeps
    then do deleteSlice slicesPath $ pack id
    else do
      slicesToDelete <- loadSliceIDsTransitive slicesPath [pack id]
      forM_ slicesToDelete (deleteSlice slicesPath)

removeEnv :: String -> Bool -> IO ()
removeEnv id nodeps = do
  putStrLn id