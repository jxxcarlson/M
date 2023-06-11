module Compiler exposing
    ( compileL
    , compileM
    , parseL
    , parseM
    , pm
    )

import Element exposing (Element)
import Generic.Acc
import Generic.Compiler
import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (ExpressionBlock)
import Generic.Pipeline
import M.ExpressionParser
import M.PrimitiveBlock
import MicroLaTeX.Expression
import MicroLaTeX.PrimitiveBlock
import Render.Block
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings
import Render.Tree


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
    Generic.Compiler.parse_ M.PrimitiveBlock.parse M.ExpressionParser.parse idPrefix outerCount lines



-- M compiler


compileM : String -> Int -> Generic.Compiler.RenderData -> List String -> List (Element MarkupMsg)
compileM idPrefix outerCount renderData lines =
    case parseM idPrefix outerCount lines of
        Err err ->
            [ Element.text "Oops something went wrong" ]

        Ok forest_ ->
            let
                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_

                _ =
                    accumulator
            in
            Generic.Forest.map (Render.Block.render renderData.count accumulator renderData.settings) forest
                |> List.map Render.Tree.unravel



-- LaTeX compiler


{-|

    > pl str = parseL "!!" (String.lines str) |> Result.map (F.map simplifyExpressionBlock)

-}
parseL : String -> Int -> List String -> Result Error (Forest ExpressionBlock)
parseL idPrefix outerCount lines =
    Generic.Compiler.parse_ MicroLaTeX.PrimitiveBlock.parse MicroLaTeX.Expression.parse idPrefix outerCount lines


compileL : String -> Int -> Generic.Compiler.RenderData -> List String -> List (Element MarkupMsg)
compileL idPrefix outerCount renderData lines =
    -- TODO: case parseL renderData.idPrefix lines of
    case parseL idPrefix outerCount lines of
        Err err ->
            [ Element.text "Oops something went wrong" ]

        Ok forest_ ->
            let
                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_

                _ =
                    accumulator
            in
            Generic.Forest.map (Render.Block.render renderData.count accumulator renderData.settings) forest
                |> List.map Render.Tree.unravel



--
