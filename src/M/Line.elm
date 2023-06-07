module M.Line exposing
    ( Line
    , PrimitiveBlockType(..)
    , classify
    , getBlockType
    , getNameAndArgs
    , isEmpty
    , isNonEmptyBlank
    , prefixLength
    , prefixLengths
    , showBlockType
    )

import Parser exposing ((|.), (|=), Parser)


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


{-|

    - ident:      the number of blanks before the first non-blank
    - prefix:     the string of blanks preceding the first non-blank
    - content:    the original string with the prefix removed
    - lineNumber: the line number in the source text
    - position:   the position of the first character of the line in the source text

-}
type alias Line =
    { indent : Int, prefix : String, content : String, lineNumber : Int, position : Int }


type PrimitiveBlockType
    = PBVerbatim
    | PBOrdinary
    | PBParagraph


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


getBlockType : String -> PrimitiveBlockType
getBlockType line_ =
    let
        line =
            String.trim line_
    in
    if String.left 2 line == "||" then
        PBVerbatim

    else if String.left 2 line == "$$" then
        PBVerbatim

    else if
        String.left 1 line
            == "|"
    then
        PBOrdinary

    else
        PBParagraph


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
    Parser.succeed (\prefixStart prefixEnd lineEnd content -> { indent = prefixEnd - prefixStart, prefix = String.slice 0 prefixEnd content, content = String.slice prefixEnd lineEnd content, position = position, lineNumber = lineNumber })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '\n')
        |= Parser.getOffset
        |= Parser.getSource
