module Generic.BlockUtilities exposing
    ( argsAndProperties
    , dropLast
    , getExpressionBlockName
    , getPrimitiveBlockName
    , updateMeta
    )

import Dict exposing (Dict)
import Generic.Language exposing (BlockMeta, ExpressionBlock, Heading(..), PrimitiveBlock)
import Tools.KV as KV


getLineNumber : { a | meta : BlockMeta } -> Int
getLineNumber b =
    b.meta.lineNumber


setLineNumber : Int -> { a | meta : BlockMeta } -> { a | meta : BlockMeta }
setLineNumber k b =
    updateMeta (\m -> { m | lineNumber = k }) b


updateMeta : (BlockMeta -> BlockMeta) -> { a | meta : BlockMeta } -> { a | meta : BlockMeta }
updateMeta transformMeta block =
    let
        oldMeta =
            block.meta

        newMeta =
            transformMeta oldMeta
    in
    { block | meta = newMeta }


argsAndProperties : List String -> ( List String, Dict String String )
argsAndProperties words =
    let
        args =
            KV.cleanArgs words

        namedArgs =
            List.drop (List.length args) words

        properties =
            namedArgs |> KV.prepareList |> KV.prepareKVData
    in
    ( words, properties )


getPrimitiveBlockName : PrimitiveBlock -> Maybe String
getPrimitiveBlockName block =
    case block.heading of
        Paragraph ->
            Nothing

        Ordinary name ->
            Just name

        Verbatim name ->
            Just name


getExpressionBlockName : ExpressionBlock -> Maybe String
getExpressionBlockName block =
    case block.heading of
        Paragraph ->
            Nothing

        Ordinary name ->
            Just name

        Verbatim name ->
            Just name


dropLast : List a -> List a
dropLast list =
    let
        n =
            List.length list
    in
    List.take (n - 1) list
