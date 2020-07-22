{-# LANGUAGE OverloadedStrings #-}
module Update (update) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad (forM_)
import Fragnix.Update (getUpdates, Update(..))

update :: IO ()
update = do
  Text.putStrLn "Available updates:"
  updates <- getUpdates
  forM_ updates $ \update -> do
    Text.putStr (Text.justifyLeft 25 ' ' (updateID update))
    Text.putStrLn (updateDescription update)


