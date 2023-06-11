module MicroLaTeX.ClassifyBlock exposing
    ( Classification(..)
    , LXSpecial(..)
    , classificationString
    , classify
    , contentsParser
    , getArg
    , match
    , p
    , pseudoBlockParser
    )

{-| This module provides a parser for classifying lines of LaTeX source code.

    classify : String -> Classification

-}

import Parser exposing ((|.), (|=), Parser)


type Classification
    = CBeginBlock String
    | CEndBlock String
    | CSpecialBlock LXSpecial
    | CMathBlockDelim
    | CVerbatimBlockDelim
    | CPlainText
    | CEmpty


type LXSpecial
    = LXItem
    | LXNumbered
    | LXPseudoBlock
    | LXOrdinaryBlock String
    | LXVerbatimBlock String


classifierParser : Parser Classification
classifierParser =
    Parser.oneOf
        [ beginBlockParser
        , endBlockParser
        , mathBlockDelimParser
        , verbatimBlockDelimParser
        , ordinaryBlockParser
        , verbatimBlockParser
        , itemParser
        , sectionParser
        , subsectionParser
        , subsubsectionParser
        , subheadingParser
        , setcounterParser
        , contentsParser
        , numberedParser
        ]


{-|

    For testing purposes.

-}
p str =
    Parser.run classifierParser str


classify : String -> Classification
classify str =
    let
        str_ =
            String.trimLeft str
    in
    if str_ == "" then
        CEmpty

    else
        case Parser.run classifierParser str_ of
            Ok classificationOfLine ->
                classificationOfLine

            Err _ ->
                if str == "" then
                    CEmpty

                else
                    CPlainText


itemParser : Parser Classification
itemParser =
    Parser.succeed (CSpecialBlock LXItem)
        |. Parser.symbol "\\item"


sectionParser : Parser Classification
sectionParser =
    -- TODO: more work on related variants
    specialBlockParser "section" (LXOrdinaryBlock "section")


subsectionParser : Parser Classification
subsectionParser =
    specialBlockParser "subsection" (LXOrdinaryBlock "subsection")


subsubsectionParser : Parser Classification
subsubsectionParser =
    specialBlockParser "subsubsection" (LXOrdinaryBlock "subsubsection")


subheadingParser : Parser Classification
subheadingParser =
    specialBlockParser "subheading" (LXOrdinaryBlock "subheading")


setcounterParser : Parser Classification
setcounterParser =
    specialBlockParser "setcounter" (LXOrdinaryBlock "setcounter")


contentsParser : Parser Classification
contentsParser =
    pseudoBlockParser "contents" (LXOrdinaryBlock "contents")


specialBlockParser : String -> LXSpecial -> Parser Classification
specialBlockParser name lxSpecial =
    (Parser.succeed String.slice
        |. Parser.symbol ("\\" ++ name ++ "{")
        |= Parser.getOffset
        |. Parser.chompUntil "}"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\_ -> CSpecialBlock lxSpecial)


pseudoBlockParser : String -> LXSpecial -> Parser Classification
pseudoBlockParser name lxSpecial =
    (Parser.succeed String.slice
        |. Parser.symbol ("\\" ++ name)
        |= Parser.getOffset
        |. Parser.chompUntil "\n"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\_ -> CSpecialBlock lxSpecial)


getArg name str =
    Parser.run (argParser name) str


argParser : String -> Parser String
argParser name =
    Parser.succeed String.slice
        |. Parser.symbol ("\\" ++ name ++ "{")
        |= Parser.getOffset
        |. Parser.chompUntil "}"
        |= Parser.getOffset
        |= Parser.getSource


match : Classification -> Classification -> Bool
match c1 c2 =
    case ( c1, c2 ) of
        ( CBeginBlock label1, CEndBlock label2 ) ->
            label1 == label2

        ( CMathBlockDelim, CMathBlockDelim ) ->
            True

        ( CVerbatimBlockDelim, CVerbatimBlockDelim ) ->
            False

        ( CSpecialBlock _, _ ) ->
            True

        _ ->
            False


classificationString : Classification -> String
classificationString classification =
    case classification of
        CBeginBlock name ->
            name

        CEndBlock name ->
            name

        _ ->
            "??"


mathBlockDelimParser : Parser Classification
mathBlockDelimParser =
    (Parser.succeed ()
        |. Parser.symbol "$$"
    )
        |> Parser.map (\_ -> CMathBlockDelim)


verbatimBlockDelimParser : Parser Classification
verbatimBlockDelimParser =
    (Parser.succeed ()
        |. Parser.symbol "```"
    )
        |> Parser.map (\_ -> CVerbatimBlockDelim)


beginBlockParser : Parser Classification
beginBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "\\begin{"
        |= Parser.getOffset
        |. Parser.chompUntil "}"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map CBeginBlock


numberedParser : Parser Classification
numberedParser =
    Parser.succeed (CSpecialBlock LXNumbered)
        |. Parser.symbol "\\numbered"


ordinaryBlockParser : Parser Classification
ordinaryBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "| "
        |= Parser.getOffset
        |. Parser.chompUntilEndOr " "
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> CSpecialBlock (LXOrdinaryBlock s))


verbatimBlockParser : Parser Classification
verbatimBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "|| "
        |= Parser.getOffset
        |. Parser.chompUntilEndOr " "
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> CSpecialBlock (LXVerbatimBlock s))


endBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "\\end{"
        |= Parser.getOffset
        |. Parser.chompUntil "}"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map CEndBlock
