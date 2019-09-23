module Slice exposing (..)

import Json.Encode as E
import Json.Decode as Decode
import Set exposing (Set)

-- | DEFINITION
-- | Matches Slice.hs as close as possible
type Slice = Slice SliceID Language Fragment (List Use) (List Instance)

type Language = Language (List GHCExtension)

type Fragment = Fragment (List SourceCode)

type Use = Use (Maybe Qualification) UsedName Reference

type Instance = Instance InstancePart InstanceID

type InstancePart =
    OfThisClass |
    OfThisClassForUnknownType |
    ForThisType |
    ForThisTypeOfUnknownClass

type alias InstanceID = SliceID

type Reference = OtherSlice SliceID | Builtin OriginalModule

type UsedName =
    ValueName Name |
    TypeName Name |
    ConstructorName TypeName Name

type Name = Identifier String | Operator String

type alias TypeName = Name

type alias SliceID = String
type alias SourceCode = String
type alias Qualification = String
type alias OriginalModule = String
type alias GHCExtension = String

-- | Wrapper indexing some extra information (Elm only)
type alias SliceWrap =
  { slice: Slice
  , occurences: List SliceID
  , comments: SourceCode
  , signature: SourceCode
  , name: SourceCode
  , tagline: SourceCode
  , id: SliceID
  , marked: Bool
  }

-- | HELPER FUNCTIONS

-- | all References as Tuple of used Name and referenced SliceID
extractReferences : Slice -> List (String, SliceID)
extractReferences slice =
  case slice of
    (Slice _ _ _ uses _) ->
      removeDuplicates (List.filterMap extractReference uses)

-- | all Dependencies as SliceID
extractDependencies : Slice -> List SliceID
extractDependencies slice =
  extractReferences slice
    |> List.map Tuple.second
    |> removeDuplicates

-- | Turn a Use into its used String name plus the referenced ID
-- | or Nothing if it is a builtin Module
extractReference : Use -> Maybe (String, SliceID)
extractReference use =
  case use of
    (Use _ _ (Builtin _)) -> Nothing
    (Use _ name (OtherSlice sid)) ->
      case name of
        (ValueName n) -> Just (nameToString n, sid)
        (TypeName n) -> Just (nameToString n, sid)
        (ConstructorName _ n) -> Just (nameToString n, sid)

-- | Turn a Name into a String
nameToString : Name -> String
nameToString name =
  case name of
    Identifier s -> s
    Operator s   -> s

removeDuplicates : List comparable -> List comparable
removeDuplicates list =
  Set.toList (Set.fromList list)

-- | Collect most of the extra information needed to wrap a Slice
-- | occurences still need to be added though!
wrap : Slice -> SliceWrap
wrap bare =
  case bare of
    (Slice sid _ (Fragment lines) _ _) ->
      let
        (name, signature, tagline) =
          extractNameSignatureAndTagline lines
      in
        { slice = bare
        , occurences = []
        , comments = String.concat (List.filter (String.startsWith "--") lines)
        , signature = signature
        , name = name
        , tagline = tagline
        , id = sid
        , marked = False
        }

extractNameSignatureAndTagline : List SourceCode -> (String, String, String)
extractNameSignatureAndTagline lines =
  case extractFromInstanceDeclaration lines of
    Just x -> x
    Nothing ->
      case extractFromTypeDeclaration lines of
        Just y -> y
        Nothing ->
          case extractFromValue lines of
            Just z -> z
            Nothing -> extractDefault lines

extractFromInstanceDeclaration : List SourceCode -> Maybe (String, String, String)
extractFromInstanceDeclaration lines =
  case (List.filter (String.startsWith "instance") lines) of
    inst :: _ ->
      let
        n =
          String.words inst
          |> List.map String.toList
          |> List.filter (\xs -> case xs of
                            [] -> False
                            x::_ -> Char.isUpper x)
          |> (\xs -> case xs of
                _::a::_ -> String.fromList a
                b::_    -> String.fromList b
                []       -> "")
      in
        Just (n, n, inst)
    []        -> Nothing

extractFromTypeDeclaration : List SourceCode -> Maybe (String, String, String)
extractFromTypeDeclaration lines =
  case (List.filter (\s -> (String.startsWith "data" s) ||
                           (String.startsWith "newtype" s) ||
                           (String.startsWith "type" s)) lines) of
    typ :: _ ->
      let
        n =
          String.words typ
          |> List.map String.toList
          |> List.filter (\xs -> case xs of
                            [] -> False
                            x::_ -> Char.isUpper x)
          |> (\xs -> case xs of
                b::_    -> String.fromList b
                []       -> "")
      in
        Just (n, n, typ)
    []        -> Nothing

extractFromValue : List SourceCode -> Maybe (String, String, String)
extractFromValue lines =
  case (List.filter (String.contains "=") lines) of
    val :: _ ->
      let
        n =
          case String.words val of
            x :: _ -> x
            []     -> ""
        tg =
          case List.filter (\s -> (String.startsWith n s)
                                  && (String.contains "::" s)) lines of
            t :: _ -> t
            []     -> n
        sig =
          String.words tg
          |> List.drop 2
          |> List.intersperse " "
          |> String.concat

      in
        Just (n, sig, tg)
    []        -> Nothing

extractDefault : List SourceCode -> (String, String, String)
extractDefault lines =
  case lines of
    l :: _ -> (l, l, l)
    []     -> ("", "", "")



-- | DECODERS
-- | match the encodings defined in Slice.hs

-- | Slice Decoder
sliceDecoder : Decode.Decoder Slice
sliceDecoder =
  Decode.map5 Slice
    (Decode.field "sliceID" Decode.string)
    (Decode.field "language" languageDecoder)
    (Decode.field "fragment" fragmentDecoder)
    (Decode.field "uses" (Decode.list useDecoder))
    (Decode.field "instances" (Decode.list instanceDecoder))

languageDecoder : Decode.Decoder Language
languageDecoder =
  Decode.map Language (Decode.field "extensions" (Decode.list Decode.string))

fragmentDecoder : Decode.Decoder Fragment
fragmentDecoder =
  Decode.map Fragment (Decode.list Decode.string)

useDecoder : Decode.Decoder Use
useDecoder =
  Decode.map3 Use
    (Decode.field "qualification" (Decode.nullable Decode.string))
    (Decode.field "usedName" usedNameDecoder)
    (Decode.field "reference" referenceDecoder)

usedNameDecoder : Decode.Decoder UsedName
usedNameDecoder =
  Decode.oneOf
    [ (Decode.map ValueName (Decode.field "valueName" nameDecoder))
    , (Decode.map TypeName (Decode.field "typeName" nameDecoder))
    , (Decode.map2 ConstructorName
        (Decode.field "constructorTypeName" nameDecoder)
        (Decode.field "constructorName" nameDecoder))
    ]

referenceDecoder : Decode.Decoder Reference
referenceDecoder =
  Decode.oneOf
    [ (Decode.map OtherSlice (Decode.field "otherSlice" Decode.string))
    , (Decode.map Builtin (Decode.field "builtinModule" Decode.string))
    ]

nameDecoder : Decode.Decoder Name
nameDecoder =
  Decode.oneOf
    [ (Decode.map Identifier (Decode.field "identifier" Decode.string))
    , (Decode.map Operator (Decode.field "operator" Decode.string))
    ]

instanceDecoder : Decode.Decoder Instance
instanceDecoder =
  Decode.map2 Instance
    (Decode.field "instancePart" instancePartDecoder)
    (Decode.field "instanceID" Decode.string)

instancePartDecoder : Decode.Decoder InstancePart
instancePartDecoder =
  Decode.string
    |> Decode.andThen instancePartFromString

instancePartFromString : String -> Decode.Decoder InstancePart
instancePartFromString s =
  case s of
    "OfThisClass" -> Decode.succeed OfThisClass
    "OfThisClassForUnknownType" -> Decode.succeed OfThisClassForUnknownType
    "ForThisType" -> Decode.succeed ForThisType
    "ForThisTypeOfUnknownClass" -> Decode.succeed ForThisTypeOfUnknownClass
    wrong -> Decode.fail ("Bad Value for InstancePart: " ++ wrong)
