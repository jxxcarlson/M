module M.Parser exposing (f, toExpressionBlockForestFromStringlist, toExpressionBlocksFromString)

import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (ExpressionBlock)
import Generic.Pipeline
import M.ExpressionParser
import M.PrimitiveBlockParser


{-|

    For testing things and working in the repl

-}
f : String -> Result Error (Forest Generic.Language.SimpleExpressionBlock)
f str =
    str
        |> String.lines
        |> toExpressionBlockForestFromStringlist
        |> Result.map (Generic.Forest.map Generic.Language.simplifyExpressionBlock)


toExpressionBlockForestFromStringlist : List String -> Result Error (Forest ExpressionBlock)
toExpressionBlockForestFromStringlist lines =
    lines
        |> Generic.Pipeline.toExpressionBlockForestFromStringlist M.ExpressionParser.parse


toExpressionBlocksFromString : Int -> String -> List ExpressionBlock
toExpressionBlocksFromString lineNumber str =
    str
        |> String.lines
        |> M.PrimitiveBlockParser.parse "!!"
        |> List.map (Generic.Pipeline.toExpressionBlock lineNumber M.ExpressionParser.parse)
