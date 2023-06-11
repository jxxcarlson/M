module XMarkdown.PrimitiveBlock exposing (..)

import Generic.Language exposing (PrimitiveBlock)
import Generic.PrimitiveBlock


{-| Parse a list of strings into a list of primitive blocks given
a function for determining when a string is the first line
of a verbatim block

NOTE (TODO) for the moment we assume that the input ends with
a blank line.

-}
parse : String -> Int -> List String -> List PrimitiveBlock
parse initialId outerCount lines =
    Generic.PrimitiveBlock.parse isVerbatimLine initialId outerCount lines


getBlockType : Language -> String -> PrimitiveBlockType
getBlockType lang line_ =
    let
        line =
            String.trim line_
    in
    case lang of
        L0Lang ->
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

        MicroLaTeXLang ->
            -- Note the source text has already been partially transformed to conform to L0
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

        PlainTextLang ->
            PBParagraph

        XMarkdownLang ->
            if String.left 3 line == "```" then
                PBVerbatim

            else if String.left 3 line == "|| " then
                PBVerbatim

            else if String.left 2 line == "$$" then
                PBVerbatim

            else if String.left 2 line == "| " then
                PBOrdinary

            else
                PBParagraph


isVerbatimLine : String -> Bool
isVerbatimLine str =
    (String.left 2 str == "||")
        || (String.left 3 str == "```")
        || (String.left 16 str == "\\begin{equation}")
        || (String.left 15 str == "\\begin{aligned}")
        || (String.left 15 str == "\\begin{comment}")
        || (String.left 12 str == "\\begin{code}")
        || (String.left 12 str == "\\begin{verbatim}")
        || (String.left 18 str == "\\begin{mathmacros}")
        || (String.left 14 str == "\\begin{iframe}")
        || (String.left 2 str == "$$")
