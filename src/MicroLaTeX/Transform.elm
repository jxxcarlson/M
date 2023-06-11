module MicroLaTeX.Transform exposing (handleImage, macroArg, pseudoBlockNamesWithContent, transform)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Generic.BlockUtilities
import Generic.Language exposing (ExpressionBlock, Heading(..), PrimitiveBlock)
import MicroLaTeX.Line as Line
import MicroLaTeX.Util
import Parser as P


pseudoBlockNamesWithContent =
    [ "title", "section", "subsection", "subsubsection", "subheading", "setcounter", "contents", "endnotes", "image" ]


sectionDict : Dict String String
sectionDict =
    Dict.fromList
        [ ( "section", "1" )
        , ( "subsection", "2" )
        , ( "subsubsection", "3" )
        , ( "subheading", "4" )
        ]


{-|

        The purpose of this function is to transform a primitive block
        like the one coming from a single-line paragraph with text
        "\section{Intro}" to an ordinary (blockType PBOrdinaryBlock)
        block with name "section", args ["1"], and content ["Introduction"].
        This is to coerce parsed MiniLaTeX source to our standard model.



        { indent = 0
        , lineNumber = 123
        , position = 4561
        , content = ["\section{Introduction}"]
        , name = Nothing
        , args = []
        , properties = Dict.empty
        , sourceText "\section{Introduction}"
        , blockType = PBParagraph
        }

        -->

        { indent = 0
        , lineNumber = 123
        , position = 4561
        , content = ["Introduction"]
        , name = Just "section"
        , args = ["1"]
        , properties = Dict.empty
        , sourceText "\section{Introduction}"
        , blockType = PBOrdinaryBlock
        }

-}
transform : PrimitiveBlock -> PrimitiveBlock
transform block =
    let
        normalizedContent =
            block.body
                |> List.map String.trimLeft
                |> normalize
    in
    case ( block.heading, block.body, normalizedContent ) of
        ( Verbatim _, _, _ ) ->
            block

        ( _, _, firstLine :: _ ) ->
            let
                name =
                    if String.left 1 firstLine == "\\" then
                        String.dropLeft 1 firstLine |> String.split "{" |> List.head |> Maybe.withDefault "---"

                    else
                        firstLine

                arg : Maybe String
                arg =
                    case P.run (MicroLaTeX.Util.macroValParserX name) firstLine of
                        Ok result ->
                            Just (result |> String.dropRight 1)

                        Err _ ->
                            Nothing
            in
            if List.member name pseudoBlockNamesWithContent then
                handlePseudoBlockWithContent name arg block

            else
                block

        _ ->
            block


macroArg : String -> String -> String
macroArg macroName str =
    String.replace ("\\" ++ macroName ++ "{") "" str |> String.dropRight 1


handlePseudoBlockWithContent : String -> Maybe String -> PrimitiveBlock -> PrimitiveBlock
handlePseudoBlockWithContent name maybeArg block =
    case maybeArg of
        Nothing ->
            { block
                | body = [] -- ("| section " ++ val) :: [ str ]
                , args = []
            }

        Just arg ->
            if name == "image" then
                handleImage block

            else
                case Dict.get name sectionDict of
                    Nothing ->
                        { block
                            | content = [ arg ] --("| " ++ macroName) :: [ str ]
                            , name = Just name
                            , args = [ arg ]
                            , blockType = PBOrdinary
                        }

                    Just val ->
                        { block
                            | content = [ arg ] -- ("| section " ++ val) :: [ str ]
                            , args = val :: []
                            , name = Just "section"
                            , blockType = PBOrdinary
                        }


handleImage : PrimitiveBlock -> PrimitiveBlock
handleImage block =
    let
        words =
            List.head block.body
                |> Maybe.withDefault "???"
                |> String.replace "\\image{" ""
                |> String.replace "}" ""
                |> String.words

        ( _, properties_ ) =
            Generic.BlockUtilities.argsAndProperties (List.drop 1 words)

        properties =
            properties_

        --case Dict.get "caption" properties_ of
        --    Just _ ->
        --        properties_
        --
        --    Nothing ->
        --        -- Make sure that there is a caption entry
        --        Dict.insert "caption" " " properties_
    in
    { block
        | properties = properties

        -- TODO: I doubt very much that this is correct
        , body = Left (List.take 1 words |> String.join "\n")
    }


normalize : List String -> List String
normalize list =
    case list of
        "" :: rest ->
            rest

        _ ->
            list
