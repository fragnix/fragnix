{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
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
  Slice(Slice), SliceID, UsedName(..), Name(..),
  Use(Use), Reference(OtherSlice, Builtin), Instance(Instance),
  loadSlicesTransitive, writeSlice, moduleNameReference)
import Data.Maybe (fromMaybe, maybeToList)
import Fragnix.Environment (loadEnvironment, persistEnvironment)
import Language.Haskell.Names (Environment, Symbol(..))

textToInt :: Text.Text -> Int
textToInt = read . Text.unpack

strictToLazy :: Text.Text -> TextL.Text
strictToLazy = TextL.pack . Text.unpack

printUsedName :: UsedName -> TextL.Text
printUsedName (ValueName n) = "Value: " <> printName n
printUsedName (TypeName n) = "Type: " <> printName n
printUsedName (ConstructorName n1 n2) = "Constructor: " <> printName n1 <> "." <> printName n2

printName :: Name -> TextL.Text
printName (Identifier n) = strictToLazy n
printName (Operator n) = strictToLazy n

edgemap :: Slice -> [(Int, Int, TextL.Text)]
edgemap (Slice id _ _ uses instances) = do
  Use _ usedName (OtherSlice id') <- uses
  return (textToInt id, textToInt id', printUsedName usedName)


getGraph :: IO (Gr TextL.Text TextL.Text)
getGraph = do
  env <- loadEnvironment environmentPath
  let sliceIDs = do
        symbols <- Map.elems env
        symbol <- symbols
        let ModuleName () moduleName = symbolModule symbol
        OtherSlice sliceID <- [moduleNameReference moduleName]
        return sliceID
  slices <- loadSlicesTransitive slicesPath sliceIDs
  -- Transform them into a graph.
  let nodes = fmap (\(Slice id _ _ uses instances) -> (textToInt id, strictToLazy ("F" <> id <> ".hs"))) slices
  let edges = concat $ fmap edgemap slices
  return $ mkGraph @Gr nodes edges

visualize :: IO ()
visualize = do
  graph <- getGraph

  let labelledNodesParams = nonClusteredParams { fmtNode = \(_,label) -> [Label (StrLabel label)]
                                               , fmtEdge = \(_,_,label) -> [Label (StrLabel label)]}
  let dotgraph = graphToDot labelledNodesParams graph
  let outputText = (renderDot . toDot) dotgraph
  TextL.writeFile "output.dot" outputText
  putStrLn "Output has been written to \"output.dot\"."
  putStrLn "Run \"dot -Tsvg output.dot > output.svg\" to create visualization."
