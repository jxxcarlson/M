module Generic.BlockUtilities exposing (argsAndProperties, dropLast, getName, updateMeta)

import Dict exposing (Dict)
import Generic.Language exposing (BlockMeta, Heading(..), PrimitiveBlock)
import Tools.KV as KV


updateMeta : (BlockMeta -> BlockMeta) -> PrimitiveBlock -> PrimitiveBlock
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


getName : PrimitiveBlock -> Maybe String
getName block =
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
