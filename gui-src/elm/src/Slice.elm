module Slice exposing (..)

import Json.Encode as E
import Json.Decode as Decode
import Set exposing (Set)
import Dict exposing (Dict)

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
  , signatures: List SourceCode
  , names: List SourceCode
  , tagline: SourceCode
  , id: SliceID
  , marked: Bool
  , origin: Origin
  , locals : Set SliceID
  }

type Origin
  = Disk
  | ChangedFrom SliceWrap (List Change)

type Change
  = Refactor
  | Signature
  | Name

-- | HELPER FUNCTIONS
changeNames : SliceID -> List (SourceCode, SourceCode) -> SliceWrap -> Maybe SliceWrap
changeNames reference changes sw =
  case changeReferences reference changes sw.slice of
    Nothing -> Nothing
    Just newSlice ->
      let
        newOrigin =
          case sw.origin of
            Disk -> ChangedFrom sw [Refactor]
            ChangedFrom old kinds ->
              if List.member Refactor kinds then
                sw.origin
              else
                ChangedFrom old (Refactor :: kinds)

        (names, signatures, tagline) =
          case newSlice of
            (Slice _ _ (Fragment lines) _ _) ->
              extractNamesSignaturesAndTagline lines
      in
        Just { sw |
               slice = newSlice
             , origin = newOrigin
             , names = names 
             , signatures = signatures
             , tagline = tagline
             }

changeReferences : SliceID -> List (SourceCode, SourceCode) -> Slice -> Maybe Slice
changeReferences refId changes (Slice id lang (Fragment frag) uses instances) =
  let
    changeDict =
      Dict.fromList changes
    (newUses, updates) =
      List.map (changeUse refId changeDict) uses
      |> List.unzip
      |> Tuple.mapSecond (List.filterMap identity)
  in
    if (List.isEmpty updates) then
      Nothing
    else
      let
        newFrag =
          List.map (updateNames (Dict.fromList updates)) frag
      in
        Just (Slice id lang (Fragment newFrag) newUses instances)

changeUse : SliceID -> Dict SourceCode SourceCode -> Use -> (Use, Maybe (SourceCode, SourceCode))
changeUse refId changeDict (Use qual name ref) =
  case ref of
    Builtin _ ->
      (Use qual name ref, Nothing)
    OtherSlice sid ->
      if sid /= refId then
        (Use qual name ref, Nothing)
      else
        case Dict.get (usedNameToString name) changeDict of
          Nothing ->
            (Use qual name ref, Nothing)
          Just newName ->
            let
              qualification = case qual of
                Nothing -> ""
                Just q -> q ++ "."
            in
              ( Use qual (changeUsedName name newName) ref
              , Just
                  ( qualification ++ (usedNameToString name)
                  , qualification ++ newName)
             )

changeUsedName : UsedName -> SourceCode -> UsedName
changeUsedName usedName newName =
  case usedName of
    ValueName n ->
      ValueName (changeName n newName)
    TypeName n ->
      TypeName (changeName n newName)
    ConstructorName t n ->
      ConstructorName t (changeName n newName)

changeName : Name -> SourceCode -> Name
changeName name newName =
  case name of
    Identifier _ -> Identifier newName
    Operator _   -> Operator newName

updateNames : Dict SourceCode SourceCode -> SourceCode -> SourceCode
updateNames changes line =
  String.words line
  |> List.map (\s -> case Dict.get s changes of
        Nothing  -> s
        Just new -> new)
  |> List.intersperse " "
  |> String.concat

hasChanged : SliceWrap -> Bool
hasChanged { origin, locals } =
  not (origin == Disk && Set.isEmpty locals)

mapReferenceId : (SliceID -> SliceID) -> Use -> Use
mapReferenceId f (Use qual name ref) =
  let
    newRef = case ref of
      Builtin _ -> ref
      OtherSlice id -> OtherSlice (f id)
  in
    (Use qual name newRef)

-- | all References as Tuple of used Name and referenced SliceID
-- | does currently not include instances since they do not have a used name
extractReferences : Slice -> List (String, SliceID)
extractReferences slice =
  case slice of
    (Slice _ _ _ uses _) ->
      removeDuplicates (List.filterMap extractReference uses)

-- | all Dependencies as SliceID, including uses and instances
extractDependencies : Slice -> List SliceID
extractDependencies slice =
  extractReferences slice
    |> List.map Tuple.second
    |> (\xs -> (List.map (\(Instance _ id) -> id) (getInstances slice)) ++ xs)
    |> removeDuplicates

getInstances (Slice _ _ _ _ instances) = instances

-- | Turn a Use into its used String name plus the referenced ID
-- | or Nothing if it is a builtin Module
extractReference : Use -> Maybe (String, SliceID)
extractReference use =
  case use of
    (Use _ _ (Builtin _)) -> Nothing
    (Use _ name (OtherSlice sid)) ->
      Just (usedNameToString name, sid)

usedNameToString : UsedName -> String
usedNameToString name =
  case name of
    ValueName n -> nameToString n
    TypeName n -> nameToString n
    ConstructorName _ n -> nameToString n

-- | Turn a Name into a String
nameToString : Name -> String
nameToString name =
  case name of
    Identifier s -> s
    Operator s   -> s

removeDuplicates : List comparable -> List comparable
removeDuplicates list =
  Set.toList (Set.fromList list)

-- | render the "content" of the slice
renderFragment : Slice -> String
renderFragment slice =
  case slice of
    (Slice _ _ (Fragment codes) _ _) ->
      String.concat (List.intersperse "\n" codes)

-- | Update the text in a sliceWrap, without changing the ids
changeText : String -> SliceWrap -> SliceWrap
changeText codes sw =
  case sw.slice of
    (Slice sid lang (Fragment _) uses instances) ->
      let
        lines = String.lines codes
        newSlice = Slice sid lang (Fragment lines) uses instances
        (names, signatures, tagline) =
          extractNamesSignaturesAndTagline lines
      in
        { sw | slice = newSlice
        , comments = String.concat (List.filter (String.startsWith "--") lines)
        , signatures = signatures
        , names = names
        , tagline = tagline
        , id = sid
        }

-- | Update diff after the text has changed (and not more!)
computeChangeKinds : SliceWrap -> SliceWrap -> SliceWrap
computeChangeKinds oldS new =
  let
    old = case new.origin of
      ChangedFrom sw _ ->
        sw
      _ ->
        oldS
    changes =
      []
      ++ (if (renderFragment old.slice) == (renderFragment new.slice) then [] else [Refactor])
      ++ (if equalLists old.signatures new.signatures then [] else [Signature])
      ++ (if equalLists old.names new.names then [] else [Name])
  in
    if List.isEmpty changes then
      old
    else
      { new | origin = ChangedFrom old changes }

equalLists : List comparable -> List comparable -> Bool
equalLists a b =
  List.length a == List.length b &&
    List.all identity (List.map2 (==) a b)

-- check if dependencies have changed
equalDependencies : Slice -> Slice -> Bool
equalDependencies (Slice _ _ _ usesA _) (Slice _ _ _ usesB _) =
  (containsUses usesA usesB) && (containsUses usesB usesA)

containsUses : List Use -> List Use -> Bool
containsUses xs ys =
  List.foldl
    (\x acc ->
      (List.length (List.filter (equalUse x) ys) > 0) && acc)
      True
      xs

equalUse : Use -> Use -> Bool
equalUse (Use qualA nameA refA) (Use qualB nameB refB) =
  (equalMaybe qualA qualB) && (equalUsedName nameA nameB) && (equalReference refA refB)

equalMaybe : Maybe comparable -> Maybe comparable -> Bool
equalMaybe a b =
  case (a, b) of
    (Nothing, Nothing) -> True
    (Nothing, _      ) -> False
    (_      , Nothing) -> False
    (x      , y      ) -> x == y

equalUsedName : UsedName -> UsedName -> Bool
equalUsedName a b =
  case (a, b) of
    (ValueName x, ValueName y) -> equalName x y
    (TypeName  x, TypeName  y) -> equalName x y
    ( ConstructorName x1 x2, ConstructorName y1 y2) ->
       (equalName x1 y1) && (equalName x2 y2)
    _                          -> False

equalName : Name -> Name -> Bool
equalName a b =
  case (a, b) of
     (Identifier x, Identifier y) -> x == y
     (Operator   x, Operator   y) -> x == y
     _                            -> False

equalReference : Reference -> Reference -> Bool
equalReference a b =
  case (a, b) of
    (OtherSlice x, OtherSlice y) -> x == y
    (Builtin    x, Builtin y)    -> x == y
    _                            -> False


-- | Update the id of a sliceWrap
changeId : SliceID -> SliceWrap -> SliceWrap
changeId newId sw =
  case sw.slice of
    (Slice _ lang frag uses instances) ->
      { sw | id = newId, slice = Slice newId lang frag uses instances }

-- | Collect most of the extra information needed to wrap a Slice
-- | occurences still need to be added though!
wrap : Slice -> SliceWrap
wrap bare =
  case bare of
    (Slice sid _ (Fragment lines) _ _) ->
      let
        (names, signatures, tagline) =
          extractNamesSignaturesAndTagline lines
      in
        { slice = bare
        , occurences = []
        , comments = String.concat (List.filter (String.startsWith "--") lines)
        , signatures = signatures
        , names = names
        , tagline = tagline
        , id = sid
        , marked = False
        , origin = Disk
        , locals = Set.empty
        }

extractNamesSignaturesAndTagline : List SourceCode -> (List String, List String, String)
extractNamesSignaturesAndTagline lines =
  case extractFromInstanceDeclaration lines of
    Just x -> x
    Nothing ->
      case extractFromTypeDeclaration lines of
        Just y -> y
        Nothing ->
          case extractFromClassDeclaration lines of
            Just z -> z
            Nothing ->
              case extractFromValue lines of
                Just w -> w
                Nothing -> extractDefault lines

extractFromInstanceDeclaration : List SourceCode -> Maybe (List String, List String, String)
extractFromInstanceDeclaration lines =
  case (List.filter (String.startsWith "instance") lines) of
    inst :: _ ->
      let
        names =
          List.concatMap String.lines lines
          |> List.filter (String.contains "=")
          |> List.map String.words
          |> List.filterMap List.head
        tagline =
          dropFrom "where" inst
      in
        Just (names, [], tagline)
    []        -> Nothing

extractFromTypeDeclaration : List SourceCode -> Maybe (List String, List String, String)
extractFromTypeDeclaration lines =
  case (List.filter (\s -> (String.startsWith "data" s) ||
                           (String.startsWith "newtype" s) ||
                           (String.startsWith "type" s)) lines) of
    typ :: rst ->
      let
        names =
          (typ :: rst)
          |> List.map String.words
          |> List.map (List.filter isCapitalized)
          |> List.filterMap List.head

        tagline =
          dropFrom "=" typ
      in
        Just (names, names, tagline)
    []        -> Nothing

isCapitalized : String -> Bool
isCapitalized string =
  case String.toList string of
    [] -> False
    x::_ -> Char.isUpper x

extractFromClassDeclaration : List SourceCode -> Maybe (List String, List String, String)
extractFromClassDeclaration lines =
  case (List.filter (\s -> String.startsWith "class" s) lines) of
    []         -> Nothing
    class :: _ ->
      let
        tagline = dropFrom "where" class
        className =
          String.words tagline
          |> List.filter isCapitalized
          |> (\xs -> case xs of
                [] -> ""
                c :: _ -> c)

        declarationLines =
          List.concatMap String.lines lines
          |> List.filter (String.contains "::")

        signatures =
          List.map (keepAfter "::") declarationLines

        names =
          declarationLines
          |> List.map String.words
          |> List.filterMap List.head

      in
        Just (className :: names, className :: signatures, tagline)

extractFromValue : List SourceCode -> Maybe (List String, List String, String)
extractFromValue lines =
  case (List.filter (String.contains "=") lines) of
    _ :: _ ->
      let
        names =
          declarationLines
          |> List.map String.words
          |> List.filterMap List.head

        declarationLines =
          List.filter (String.contains "::") lines

        signatures =
          List.map (keepAfter "::") declarationLines

        tg =
          [ names, signatures ]
          |> List.map List.head
          |> List.map (Maybe.withDefault "")
          |> List.intersperse " :: "
          |> String.concat

      in
        Just (names, signatures, tg)
    []        -> Nothing

extractDefault : List SourceCode -> (List String, List String, String)
extractDefault lines =
  case lines of
    l :: _ -> ([], [], l)
    []     -> ([], [], "")

dropFrom : String -> String -> String
dropFrom delimiter string =
  case String.indexes delimiter string of
    []     ->
       string
    x :: _ ->
      String.left x string

keepAfter : String -> String -> String
keepAfter delimiter string =
  case String.indexes delimiter string of
    [] ->
      string
    x :: _ ->
      String.dropLeft (x + (String.length delimiter)) string
      |> String.trimLeft


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
