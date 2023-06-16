module Render.VerbatimBlock exposing (render)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input
import Generic.ASTTools as ASTTools
import Generic.Acc exposing (Accumulator)
import Generic.Language exposing (Expr(..), Expression, ExpressionBlock, Heading(..))
import Html.Attributes
import List.Extra
import Maybe.Extra
import Render.Color as Color
import Render.Expression
import Render.Graphics
import Render.Helper
import Render.IFrame
import Render.List
import Render.Math
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (RenderSettings)
import Render.Sync
import Render.Tabular
import Render.Utility exposing (elementAttribute)
import String.Extra
import Tools.Utility as Utility


render : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg
render count acc settings block =
    case block.body of
        Right _ ->
            Element.none

        Left str ->
            case block.heading of
                Verbatim functionName ->
                    case Dict.get functionName verbatimDict of
                        Nothing ->
                            Render.Helper.noSuchVerbatimBlock functionName str

                        Just f ->
                            Element.el [ Render.Helper.selectedColor block.meta.id settings ] (f count acc settings block)

                _ ->
                    Element.none


verbatimDict : Dict String (Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg)
verbatimDict =
    Dict.fromList
        [ --( "math", Render.Math.displayedMath )
          ( "equation", Render.Math.equation )

        --, ( "aligned", Render.Math.aligned )
        --, ( "code", renderCode )
        --, ( "verse", renderVerse )
        --, ( "verbatim", renderVerbatim )
        --, ( "tabular", Render.Tabular.render )
        --, ( "hide", renderNothing )
        --, ( "texComment", renderNothing )
        --, ( "docinfo", renderNothing )
        --, ( "mathmacros", renderNothing )
        --, ( "textmacros", renderNothing )
        --
        ----, ( "datatable", Render.M.table )
        ----, ( "chart", Render.M.chart )
        --, ( "svg", Render.Graphics.svg )
        --, ( "quiver", Render.Graphics.quiver )
        --, ( "image", Render.Graphics.image2 )
        --, ( "tikz", Render.Graphics.tikz )
        --, ( "load-files", renderNothing )
        --, ( "include", renderNothing )
        --, ( "iframe", Render.IFrame.render )
        ]
