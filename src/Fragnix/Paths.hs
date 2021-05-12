module Fragnix.Paths where

import System.FilePath ((</>))

fragnixBasePath :: FilePath
fragnixBasePath = "fragnix"

configPath :: FilePath
configPath = "fragnix.yaml"

environmentPath :: FilePath
environmentPath = fragnixBasePath </> "env"

builtinEnvironmentPath :: FilePath
builtinEnvironmentPath = fragnixBasePath </> "builtin_environment"

slicesPath :: FilePath
slicesPath = fragnixBasePath </> "slices"

foreignSlicesPath :: FilePath
foreignSlicesPath = fragnixBasePath </> "foreign"

foreignCodeExtensions :: [String]
foreignCodeExtensions = [".c", ".h", ".js"]

cbitsPath :: FilePath
cbitsPath = fragnixBasePath </> "cbits"

includePath :: FilePath
includePath = fragnixBasePath </> "include"

preprocessedPath :: FilePath
preprocessedPath = fragnixBasePath </> "temp" </> "preprocessed"

-- | Directory for generated modules
compilationunitsPath :: FilePath
compilationunitsPath = fragnixBasePath </> "temp" </> "compilationunits"

buildPath :: FilePath
buildPath = fragnixBasePath </> "temp" </> "build"

-- | Directory for updates
updatePath :: FilePath
updatePath = fragnixBasePath </> "updates"

declarationsPath :: FilePath
declarationsPath = fragnixBasePath </> "temp" </> "declarations/declarations.json"

