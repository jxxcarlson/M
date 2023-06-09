module Compiler exposing
    ( cm
    , compileMF
    , compileML
    )

import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (ExpressionBlock)
import Generic.Pipeline
import M.ExpressionParser
import M.PrimitiveBlockParser


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
cm str =
    compileMF "!!" (String.lines str) |> Result.map (Generic.Forest.map Generic.Language.simplifyExpressionBlock)


compileML : String -> List String -> List ExpressionBlock
compileML idPrefix lines =
    lines
        |> M.PrimitiveBlockParser.parse idPrefix
        |> List.map (Generic.Pipeline.toExpressionBlock 0 M.ExpressionParser.parse)


compileMF : String -> List String -> Result Error (Forest ExpressionBlock)
compileMF idPrefix lines =
    lines
        |> M.PrimitiveBlockParser.parse idPrefix
        |> Generic.Pipeline.toPrimitiveBlockForest
        |> Result.map (Generic.Forest.map (Generic.Pipeline.toExpressionBlock 0 M.ExpressionParser.parse))
