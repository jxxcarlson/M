module M.Print exposing (print)

{-| Used for debugging with CLI.LOPB
-}

import Dict exposing (Dict)
import M.Language exposing (BlockMeta, Heading(..), PrimitiveBlock, emptyBlockMeta)


print : PrimitiveBlock -> String
print block =
    [ "BLOCK:"
    , "Heading: " ++ displayHeading block.heading
    , "Indent: " ++ String.fromInt block.indent
    , "Args: " ++ showArgs block.args
    , "Properties: " ++ showProperties block.properties
    , "Content: "
        ++ (block.content
                |> List.indexedMap (\k s -> String.padLeft 3 ' ' (String.fromInt (k + 1 + block.meta.lineNumber)) ++ ": " ++ s)
                |> String.join "\n"
           )
    , "---------"
    , "MetaData:"
    , "    Id: " ++ block.meta.id
    , "    Position: " ++ String.fromInt block.meta.position
    , "    Line number: " ++ String.fromInt block.meta.lineNumber
    , "    Number of lines: " ++ String.fromInt block.meta.numberOfLines
    , "    messages: " ++ String.join ", " block.meta.messages
    , "    Error: " ++ showError block.meta.error
    , "    Source text:\n--------" ++ block.meta.sourceText
    , "--------"
    ]
        |> String.join "\n"


displayHeading : Heading -> String
displayHeading heading =
    case heading of
        Paragraph ->
            "Paragraph"

        Ordinary name ->
            "OrdinaryBlock: " ++ name

        Verbatim name ->
            "VerbatimBlock: " ++ name


showProperties : Dict String String -> String
showProperties dict =
    dict |> Dict.toList |> List.map (\( k, v ) -> k ++ ": " ++ v) |> String.join ", "


showArgs : List String -> String
showArgs args =
    args |> String.join ", "


showError : Maybe String -> String
showError mError =
    case mError of
        Nothing ->
            "none"

        Just error ->
            error


showName : Maybe String -> String
showName mstr =
    case mstr of
        Nothing ->
            "(anon)"

        Just name ->
            name


getName : PrimitiveBlock -> Maybe String
getName block =
    case block.heading of
        Paragraph ->
            Nothing

        Ordinary name ->
            Just name

        Verbatim name ->
            Just name
