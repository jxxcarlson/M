module Generic.Pipeline exposing (toExpressionBlock, toExpressionBlocksFromString)

import Generic.Language exposing (Expression, ExpressionBlock, Heading(..), PrimitiveBlock)
import M.ExpressionParser
import M.PrimitiveBlockParser


toExpressionBlock : String -> Int -> (Int -> String -> List Expression) -> PrimitiveBlock -> ExpressionBlock
toExpressionBlock idPrefix lineNumber parser block =
    Generic.Language.toExpressionBlock (parser lineNumber) block



-- NOT GENERIC


toExpressionBlocksFromString : String -> List ExpressionBlock
toExpressionBlocksFromString str =
    str
        |> String.lines
        |> M.PrimitiveBlockParser.parse "!!"
        |> List.map (toExpressionBlock "!!" 0 M.ExpressionParser.parse)
