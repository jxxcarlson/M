module Compiler exposing
    ( compileL
    , compileM
    , compileX
    , parseL
    , parseM
    , parseX
    , pm
    )

import Config
import Element exposing (Element)
import Generic.Acc
import Generic.Compiler
import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (ExpressionBlock)
import M.Expression
import M.PrimitiveBlock
import MicroLaTeX.Expression
import MicroLaTeX.PrimitiveBlock
import Render.Block
import Render.Msg exposing (MarkupMsg(..))
import Render.TOC
import Render.Tree
import XMarkdown.Expression
import XMarkdown.PrimitiveBlock


{-|

    > cm "hello!\n\n  [b how are you?]\n\n  $x^2 = 7$\n\n"
    Ok [ Tree { args = [], body = Right [Text "hello!" ()], firstLine = "hello!", heading = Paragraph
           , indent = 0, meta = () , properties = Dict.fromList [] }
         [ Tree { args = [], body = Right [Text ("   ") (),Fun "b" [Text (" how are you?") ()] ()]
            , firstLine = "[b how are you?]", heading = Paragraph, indent = 2, meta = ()
            , properties = Dict.fromList [] } []
         , Tree { args = [], body = Right [Text ("   ") (),VFun "math" ("x^2 = 7") ()]
           , firstLine = "$x^2 = 7$", heading = Paragraph, indent = 2, meta = ()
           , properties = Dict.fromList [] } []]]

    -- proof that the output is a one-tree forest
    > cm "hello!\n\n  [b how are you?]\n\n  $x^2 = 7$\n\n" |> Result.map List.length
    Ok 1

-}
pm str =
    parseM "!!" 0 (String.lines str) |> Result.map (Generic.Forest.map Generic.Language.simplifyExpressionBlock)


parseM : String -> Int -> List String -> Result Error (Forest ExpressionBlock)
parseM idPrefix outerCount lines =
    Generic.Compiler.parse_ M.PrimitiveBlock.parse M.Expression.parse idPrefix outerCount lines


parseX : String -> Int -> List String -> Result Error (Forest ExpressionBlock)
parseX idPrefix outerCount lines =
    Generic.Compiler.parse_ XMarkdown.PrimitiveBlock.parse XMarkdown.Expression.parse idPrefix outerCount lines



-- M compiler


compileM : Int -> Int -> String -> List String -> { body : List (Element MarkupMsg), toc : List (Element MarkupMsg) }
compileM width outerCount selectedId lines =
    case parseM Config.idPrefix outerCount lines of
        Err err ->
            { body = [ Element.text "Oops something went wrong" ], toc = [] }

        Ok forest_ ->
            let
                renderData =
                    Generic.Compiler.defaultRenderData width outerCount selectedId

                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_
            in
            { body =
                Generic.Forest.map (Render.Block.render renderData.count accumulator renderData.settings) forest
                    |> List.map Render.Tree.unravel
            , toc =
                Render.TOC.view renderData.count accumulator forest
            }



-- makeSettings id selectedSlug scale windowWidth


compileX : Int -> Int -> String -> List String -> { body : List (Element MarkupMsg), toc : List (Element MarkupMsg) }
compileX width outerCount selectedId lines =
    case parseX Config.idPrefix outerCount lines of
        Err err ->
            { body = [ Element.text "Oops something went wrong" ], toc = [] }

        Ok forest_ ->
            let
                renderData =
                    Generic.Compiler.defaultRenderData width outerCount selectedId

                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_
            in
            { body =
                Generic.Forest.map (Render.Block.render renderData.count accumulator renderData.settings) forest
                    |> List.map Render.Tree.unravel
            , toc =
                Render.TOC.view renderData.count accumulator forest
            }



-- LaTeX compiler


{-|

    > pl str = parseL "!!" (String.lines str) |> Result.map (F.map simplifyExpressionBlock)

-}
parseL : String -> Int -> List String -> Result Error (Forest ExpressionBlock)
parseL idPrefix outerCount lines =
    Generic.Compiler.parse_ MicroLaTeX.PrimitiveBlock.parse MicroLaTeX.Expression.parse idPrefix outerCount lines


compileL : Int -> Int -> String -> List String -> { body : List (Element MarkupMsg), toc : List (Element MarkupMsg) }
compileL width outerCount selectedId lines =
    case parseL Config.idPrefix outerCount lines of
        Err err ->
            { body = [ Element.text "Oops something went wrong" ], toc = [] }

        Ok forest_ ->
            let
                renderData =
                    Generic.Compiler.defaultRenderData width outerCount selectedId

                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_
            in
            { body =
                Generic.Forest.map (Render.Block.render renderData.count accumulator renderData.settings) forest
                    |> List.map Render.Tree.unravel
            , toc =
                Render.TOC.view renderData.count accumulator forest
            }



--
