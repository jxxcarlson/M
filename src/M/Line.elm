module M.Line exposing
    ( HeadingError(..)
    , Line
    , PrimitiveBlockType(..)
    , classify
    , getHeadingData
    , isEmpty
    , isNonEmptyBlank
    )

import Dict exposing (Dict)
import M.Language exposing (Heading(..))
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


type PrimitiveBlockType
    = PBVerbatim
    | PBOrdinary
    | PBParagraph


getNameAndArgs : { a | content : String } -> ( Maybe String, List String )
getNameAndArgs line =
    let
        normalizedLine =
            String.trim line.content

        -- account for possible indentation
    in
    if String.left 2 normalizedLine == "||" then
        let
            words =
                String.words (String.dropLeft 3 normalizedLine)

            name =
                List.head words |> Maybe.withDefault "anon"

            args =
                List.drop 1 words
        in
        ( Just name, args )

    else if String.left 1 normalizedLine == "|" then
        let
            words =
                String.words (String.dropLeft 2 normalizedLine)

            name =
                List.head words |> Maybe.withDefault "anon"

            args =
                List.drop 1 words
        in
        ( Just name, args )

    else if String.left 2 line.content == "$$" then
        ( Just "math", [] )

    else
        ( Nothing, [] )


showBlockType : PrimitiveBlockType -> String
showBlockType blockType =
    case blockType of
        PBVerbatim ->
            "Verbatim"

        PBOrdinary ->
            "Ordinary"

        PBParagraph ->
            "Paragraph"


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


getArgs : String -> String -> List String
getArgs prefix str =
    str |> String.replace prefix "" |> String.trim |> String.words


getHeadingData : String -> Result HeadingError { heading : M.Language.Heading, args : List String, properties : Dict String String }
getHeadingData line_ =
    let
        line =
            String.trim line_

        ( args1, properties ) =
            KV.argsAndProperties (String.words line)
    in
    case args1 of
        [] ->
            Err <| HEMissingPrefix

        prefix :: args ->
            case prefix of
                "||" ->
                    case args1 of
                        [] ->
                            Err <| HEMissingName

                        name :: args2 ->
                            Ok <| { heading = Verbatim name, args = args2, properties = properties }

                "|" ->
                    case args1 of
                        [] ->
                            Err <| HEMissingName

                        name :: args2 ->
                            Ok <| { heading = Ordinary name, args = args2, properties = properties }

                "$$" ->
                    Ok <| { heading = Verbatim "math", args = [], properties = Dict.empty }

                _ ->
                    Ok <| { heading = Paragraph, args = [], properties = Dict.empty }


prefixLength : Int -> Int -> String -> Int
prefixLength position lineNumber str =
    classify position lineNumber str |> .indent


prefixLengths : Int -> Int -> List String -> List Int
prefixLengths position lineNumber strs =
    strs |> List.map (prefixLength position lineNumber) |> List.filter (\n -> n /= 0)


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
