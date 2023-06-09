module M.Line exposing
    ( HeadingError(..)
    , Line
    , PrimitiveBlockType(..)
    , classify
    , errorToString
    , getHeadingData
    , isEmpty
    , isNonEmptyBlank
    )

import Dict exposing (Dict)
import Generic.Language exposing (Heading(..))
import M.Regex
import Parser exposing ((|.), (|=), Parser)
import Tools.KV as KV


{-|

    - ident:      the number of blanks before the first non-blank
    - prefix:     the string of blanks preceding the first non-blank
    - content:    the original string with the prefix removed
    - lineNumber: the line number in the source text
    - position:   the position of the first character of the line in the source text

-}
type alias Line =
    { indent : Int, prefix : String, content : String, lineNumber : Int, position : Int }


type HeadingError
    = HEMissingPrefix
    | HEMissingName
    | HENoContent


errorToString : HeadingError -> String
errorToString error =
    case error of
        HEMissingPrefix ->
            "Missing prefix"

        HEMissingName ->
            "Missing name"

        HENoContent ->
            "No content"


type PrimitiveBlockType
    = PBVerbatim
    | PBOrdinary
    | PBParagraph


isEmpty : Line -> Bool
isEmpty line =
    line.indent == 0 && line.content == ""


isNonEmptyBlank : Line -> Bool
isNonEmptyBlank line =
    line.indent > 0 && line.content == ""


classify : Int -> Int -> String -> Line
classify position lineNumber str =
    case Parser.run (prefixParser position lineNumber) str of
        Err _ ->
            { indent = 0, content = "!!ERROR", prefix = "", position = position, lineNumber = lineNumber }

        Ok result ->
            result


getHeadingData : String -> Result HeadingError { heading : Generic.Language.Heading, args : List String, properties : Dict String String }
getHeadingData line_ =
    let
        line =
            String.trim line_

        ( args1, properties ) =
            KV.argsAndProperties (String.words line)
    in
    case M.Regex.findTitlePrefix line of
        Just prefix ->
            { heading = Ordinary "section", args = [ String.length prefix |> String.fromInt ], properties = Dict.singleton "section-type" "markdown" }
                |> Ok

        Nothing ->
            case args1 of
                [] ->
                    Err <| HEMissingPrefix

                prefix :: args ->
                    case prefix of
                        "||" ->
                            case args of
                                [] ->
                                    Err <| HEMissingName

                                name :: args2 ->
                                    Ok <| { heading = Verbatim name, args = args2, properties = properties }

                        "|" ->
                            case args of
                                [] ->
                                    Err <| HEMissingName

                                name :: args2 ->
                                    Ok <| { heading = Ordinary name, args = args2, properties = properties }

                        "-" ->
                            let
                                reducedLine =
                                    String.replace "- " "" line
                            in
                            if String.isEmpty reducedLine then
                                Err HENoContent

                            else
                                Ok <|
                                    { heading = Ordinary "item"
                                    , args = []
                                    , properties = Dict.singleton "firstLine" (String.replace "- " "" line)
                                    }

                        "." ->
                            let
                                reducedLine =
                                    String.replace ". " "" line
                            in
                            if String.isEmpty reducedLine then
                                Err HENoContent

                            else
                                Ok <|
                                    { heading = Ordinary "numbered"
                                    , args = []
                                    , properties = Dict.singleton "firstLine" (String.replace ". " "" line)
                                    }

                        "$$" ->
                            Ok <| { heading = Verbatim "math", args = [], properties = Dict.empty }

                        _ ->
                            Ok <| { heading = Paragraph, args = [], properties = Dict.empty }


{-|

    The prefix is the first word of the line

-}
prefixParser : Int -> Int -> Parser Line
prefixParser position lineNumber =
    Parser.succeed
        (\prefixStart prefixEnd lineEnd content ->
            { indent = prefixEnd - prefixStart
            , prefix = String.slice 0 prefixEnd content
            , content = String.slice prefixEnd lineEnd content
            , position = position
            , lineNumber = lineNumber
            }
        )
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '\n')
        |= Parser.getOffset
        |= Parser.getSource
