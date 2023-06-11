module MicroLaTeX.ClassifyBlock exposing
    ( Classification(..)
    , LXSpecial(..)
    , classificationString
    , classify
    , getArg
    , match
    , p
    )

import Parser exposing ((|.), (|=), Parser)


p str =
    Parser.run classifierParser str


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


itemParser : Parser Classification
itemParser =
    Parser.succeed (CSpecialBlock LXItem)
        |. Parser.symbol "\\item"


sectionParser : Parser Classification
sectionParser =
    -- TODO: more work on related variants
    specialBlockParser "section" (LXOrdinaryBlock "section")


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

        --, pseudoBlockParser
        , numberedParser
        ]


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


pseudoBlockParser : Parser Classification
pseudoBlockParser =
    Parser.oneOf
        [ Parser.symbol "\\section" |> (\_ -> Parser.succeed (CSpecialBlock (LXOrdinaryBlock "section")))
        , Parser.symbol "\\subsection" |> (\_ -> Parser.succeed (CSpecialBlock (LXOrdinaryBlock "subsection")))
        , Parser.symbol "\\subsubsection" |> (\_ -> Parser.succeed (CSpecialBlock (LXOrdinaryBlock "subsubsubsection")))
        , Parser.symbol "\\item" |> (\_ -> Parser.succeed (CSpecialBlock (LXOrdinaryBlock "item mmmm")))
        , Parser.symbol "\\image" |> (\_ -> Parser.succeed (CSpecialBlock (LXOrdinaryBlock "image")))
        , Parser.symbol "\\title" |> (\_ -> Parser.succeed (CSpecialBlock (LXOrdinaryBlock "title")))
        , Parser.symbol "\\contents" |> (\_ -> Parser.succeed (CSpecialBlock (LXOrdinaryBlock "contents")))
        , Parser.symbol "\\setcounter" |> (\_ -> Parser.succeed (CSpecialBlock (LXOrdinaryBlock "setcounter")))
        ]


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
