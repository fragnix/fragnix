module SyntaxHighlight.Language.Type exposing (Syntax(..), Token)


type alias Token a =
    ( Syntax a, String )


type Syntax a
    = Normal
    | Comment
    | LineBreak
    | C a
