module Compiler exposing
    ( DisplaySettings
    , cm
    , compileMF
    , compileML
    , defaultRenderData
    , render
    )

import Element exposing (Element)
import Generic.Acc
import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (ExpressionBlock)
import Generic.Pipeline
import M.ExpressionParser
import M.PrimitiveBlockParser
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


type alias RenderData =
    { count : Int
    , idPrefix : String
    , settings : Render.Settings.Settings
    , initialAccumulatorData : Generic.Acc.InitialAccumulatorData
    }


defaultRenderData : RenderData
defaultRenderData =
    { count = 0
    , idPrefix = "!!"
    , settings = Render.Settings.defaultSettings
    , initialAccumulatorData = Generic.Acc.initialData
    }


render : RenderData -> List String -> List (Element MarkupMsg)
render renderData lines =
    case compileMF renderData.idPrefix lines of
        Err err ->
            [ Element.text "Oops something went wrong" ]

        Ok forest_ ->
            let
                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_
            in
            Generic.Forest.map (Render.Block.render renderData.count accumulator renderData.settings) forest
                |> List.map Render.Tree.unravel


type alias DisplaySettings =
    { windowWidth : Int
    , longEquationLimit : Float
    , counter : Int
    , selectedId : String
    , selectedSlug : Maybe String
    , scale : Float
    }
