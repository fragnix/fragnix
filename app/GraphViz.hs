{-# LANGUAGE TypeApplications #-}
module GraphViz
  ( visualize
  ) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.IO as TextL
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (graphToDot, nonClusteredParams, fmtNode, fmtEdge)
import Data.GraphViz.Attributes.Complete   (Label(StrLabel), Attribute(Label))
import Data.GraphViz.Printing (toDot, renderDot)

import Data.Map (Map)
import qualified Data.Map as Map (lookup, insert, fromList, toList, elems, empty)
import Language.Haskell.Exts.Syntax (ModuleName(..))
import Fragnix.Paths (updatePath, environmentPath, slicesPath)
import Fragnix.Slice (
  Slice(Slice), SliceID,
  Use(Use), Reference(OtherSlice, Builtin), Instance(Instance),
  moduleNameSliceID, loadSlicesTransitive, writeSlice)
import Data.Maybe (fromMaybe, maybeToList)
import Fragnix.Environment (loadEnvironment, persistEnvironment)
import Language.Haskell.Names (Environment, Symbol(..))

edgemap :: Slice -> [(Int, Int, TextL.Text)]
edgemap (Slice id _ _ uses instances) = do
  Use _ usedName (OtherSlice id') <- uses
  return (read (Text.unpack id), read (Text.unpack id'), TextL.pack $ show usedName)

visualize :: IO ()
visualize = do
  -- Get all transitively reachable slices from the environment
  env <- loadEnvironment environmentPath
  let sliceIDs = do
        symbols <- Map.elems env
        symbol <- symbols
        let ModuleName () moduleName = symbolModule symbol
        maybeToList (moduleNameSliceID moduleName)
  slices <- loadSlicesTransitive slicesPath sliceIDs
  -- Transform them into a graph.
  let nodes = fmap (\(Slice id _ _ uses instances) -> (read (Text.unpack id),TextL.pack (Text.unpack id))) slices
  let edges = concat $ fmap edgemap slices
  let graph = mkGraph @Gr nodes edges
  let labelledNodesParams = nonClusteredParams { fmtNode = \(_,label) -> [Label (StrLabel label)]
                                               , fmtEdge = \(_,_,label) -> [Label (StrLabel label)]}
  let dotgraph = graphToDot labelledNodesParams graph
  let outputText = (renderDot . toDot) dotgraph
  TextL.writeFile "output.dot" outputText
  putStrLn "Output has been written to \"output.dot\"."
  putStrLn "Run \"dot -Tsvg output.dot > output.svg\" to create visualization."
