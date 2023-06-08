module M.Regex exposing (findTitlePrefix)

import Regex


titlePrefixRegex : Regex.Regex
titlePrefixRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^#+\\s*"


findTitlePrefix : String -> Maybe String
findTitlePrefix string =
    Regex.find titlePrefixRegex string
        |> List.map .match
        |> List.head
        |> Maybe.map String.trim
