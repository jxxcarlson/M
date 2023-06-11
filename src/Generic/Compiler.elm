module Generic.Compiler exposing (DisplaySettings, RenderData, defaultRenderData, parse_)

import Generic.Acc
import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (ExpressionBlock)
import Generic.Pipeline
import Render.Settings


{-|

    This is a generic compiler from source text to HTML that
    takes two parsers as arguments. The first parser parses
    the primitive blocks, and the second parser parses the
    expressions in the blocks.

-}
parse_ :
    (String -> List String -> List Generic.Language.PrimitiveBlock)
    -> (Int -> String -> List Generic.Language.Expression)
    -> String
    -> List String
    -> Result Error (Forest ExpressionBlock)
parse_ primitiveBlockParser exprParser idPrefix lines =
    lines
        |> primitiveBlockParser idPrefix
        |> Generic.Pipeline.toPrimitiveBlockForest
        |> Result.map (Generic.Forest.map (Generic.Pipeline.toExpressionBlock 0 exprParser))


type alias RenderData =
    { count : Int
    , idPrefix : String
    , settings : Render.Settings.Settings
    , initialAccumulatorData : Generic.Acc.InitialAccumulatorData
    }


defaultRenderData : RenderData
defaultRenderData =
    { count = 0
    , idPrefix = "!@!"
    , settings = Render.Settings.defaultSettings
    , initialAccumulatorData = Generic.Acc.initialData
    }


type alias DisplaySettings =
    { windowWidth : Int
    , longEquationLimit : Float
    , counter : Int
    , selectedId : String
    , selectedSlug : Maybe String
    , scale : Float
    }
