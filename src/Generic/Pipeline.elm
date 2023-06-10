module Generic.Pipeline exposing
    ( toExpressionBlock
    , toExpressionBlockForestFromStringlist
    , toPrimitiveBlockForest
    )

import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (Expression, ExpressionBlock, PrimitiveBlock)
import M.PrimitiveBlock


toExpressionBlockForestFromStringlist : (Int -> String -> List Expression) -> List String -> Result Error (Forest ExpressionBlock)
toExpressionBlockForestFromStringlist parser lines =
    lines
        |> M.PrimitiveBlock.parse "!!"
        |> toPrimitiveBlockForest
        |> Result.map (Generic.Forest.map (toExpressionBlock 0 parser))


toExpressionBlock : Int -> (Int -> String -> List Expression) -> PrimitiveBlock -> ExpressionBlock
toExpressionBlock lineNumber parser block =
    Generic.Language.toExpressionBlock (parser lineNumber) block


toPrimitiveBlockForest : List PrimitiveBlock -> Result Error (Forest PrimitiveBlock)
toPrimitiveBlockForest blocks =
    Generic.ForestTransform.forestFromBlocks { emptyBlock | indent = -2 } .indent blocks


emptyBlock : PrimitiveBlock
emptyBlock =
    { emptyBlock_ | indent = -2 }


emptyBlock_ : PrimitiveBlock
emptyBlock_ =
    Generic.Language.primitiveBlockEmpty
