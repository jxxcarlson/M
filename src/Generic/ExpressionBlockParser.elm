module M.ExpressionBlockParser exposing (toExpressionBlock)

import Either exposing (Either(..))
import M.ExpressionParser
import M.Language exposing (ExpressionBlock, PrimitiveBlock, Heading(..)
import M.PrimitiveBlockParser


toExpressionBlock : Int -> PrimitiveBlock -> ExpressionBlock
toExpressionBlock lineNumber block =
    case block.heading of
        Paragraph ->
             {block | body = Right <| M.ExpressionParser.parse lineNumber (String.join "\n" block.lines)}
        Ordinary _ ->
            {block | body = Right <| M.ExpressionParser.parse lineNumber (String.join "\n" block.lines)}
        Verbatim _ ->
            {block | body = Left <| (String.join "\n" block.lines)}
