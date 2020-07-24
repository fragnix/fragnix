{-# LANGUAGE OverloadedStrings #-}

-- Run with: stack runghc extract_cbits.hs

import Turtle
import qualified Data.Text as Text
import Control.Monad (forM)

main :: IO ()
main = do
  putStrLn "Downloading cbits ..."
  
  -- # network package (but is phased out, see the comment in ancilData.c"
  -- ancilData.c
  --downloadFileFromGit "github.com/network.git" "abcdefg1234567" "ancilData.c"
  -- # text package
  downloadFileFromGit "https://github.com/haskell/text.git" ["cbits/cbits.c"]
  -- # unix-time package
  downloadFileFromGit "https://github.com/kazu-yamamoto/unix-time.git" ["cbits/conv.c"]
  -- # unix package
  downloadFileFromGit "https://github.com/haskell/unix.git" ["cbits/execvpe.c"]
  -- # hashable package
  downloadFileFromGit "https://github.com/tibbe/hashable.git" ["cbits/fnv.c"]
  -- # bytestring package
  downloadFileFromGit "https://github.com/haskell/bytestring.git" ["cbits/fpstring.c", "cbits/itoa.c"]
  -- # network package
  downloadFileFromGit "https://github.com/haskell/network.git" ["cbits/HsNet.c"]
  -- # time package
  downloadFileFromGit "https://github.com/haskell/time.git" ["lib/cbits/HsTime.c"]
  -- # unix package
  downloadFileFromGit "https://github.com/haskell/unix.git" ["cbits/HsUnix.c"]
  -- # unix-compat package
  downloadFileFromGit "https://github.com/jacobstanley/unix-compat.git" ["cbits/HsUnixCompat.c"]
  -- # regex-posix package
  downloadFileFromGit "https://github.com/haskell-hvr/regex-posix.git" ["cbits/myfree.c"]
  -- # primitive package
  downloadFileFromGit "https://github.com/haskell/primitive.git" ["cbits/primitive-memops.c"]
  -- # process package
  downloadFileFromGit "https://github.com/haskell/process.git"["cbits/posix/runProcess.c"]
  -- # old-time package
  downloadFileFromGit "https://github.com/haskell/old-time.git" ["cbits/timeUtils.c"]
  -- # streaming-commons package
  downloadFileFromGit "https://github.com/fpco/streaming-commons.git"["cbits/zlib-helper.c", "cbits/text-helper.c"]

downloadFileFromGit :: Text -- The source repo
                    -> [Text] -- The path to the files you want to download
                    -> IO ()
downloadFileFromGit repo files = do
  putStrLn $ Text.unpack $  "    Downloading " <> Text.concat files <> " from " <> repo
  mktree "temp"
  shells ("git clone " <> repo <> " temp") empty
  forM files $ \file -> cp (fromText ("temp/" <> file)) ("cbits" </> (filename (fromText file)))
  rmtree "temp"

