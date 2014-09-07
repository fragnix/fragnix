module Fragnix.Slice where

import Language.Haskell.Exts.Syntax (Name)

import Data.Text (Text)

data Slice = Slice SliceID Fragment [Usage]

data Fragment = Binding Signature SourceCode

data Usage = Usage (Maybe Qualification) UsedName Reference

data UsedName = Variable Name
              | Abstract Name

data Reference = OtherSlice SliceID | Primitive OriginalModule

type SliceID = Integer
type Signature = SourceCode
type SourceCode = Text
type Qualification = Text
type OriginalModule = Text


