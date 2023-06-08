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


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string
