module Generic.ExpressionBlockParser exposing (toExpressionBlock)

import Generic.Language exposing (ExpressionBlock, Heading(..), PrimitiveBlock)
import M.ExpressionParser


toExpressionBlock : Int -> PrimitiveBlock -> ExpressionBlock
toExpressionBlock lineNumber block =
    Generic.Language.toExpressionBlock (M.ExpressionParser.parse lineNumber) block
