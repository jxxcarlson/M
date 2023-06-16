module Render.Block2 exposing (renderAttributes, renderBody)

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
import Render.OrdinaryBlock as OrdinaryBlock
import Render.Settings exposing (RenderSettings)
import Render.Sync
import Render.Tabular
import Render.Utility exposing (elementAttribute)
import Render.VerbatimBlock as VerbatimBlock
import String.Extra
import Tools.Utility as Utility


renderAttributes : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> List (Element.Attribute MarkupMsg)
renderAttributes count accumulator settings block =
    case block.heading of
        Paragraph ->
            standardAttributes settings block

        Ordinary name ->
            standardAttributes settings block ++ OrdinaryBlock.attributes name

        Verbatim _ ->
            standardAttributes settings block


standardAttributes settings block =
    [ Render.Utility.idAttributeFromInt block.meta.lineNumber
    , Render.Sync.rightToLeftSyncHelper block.meta.lineNumber block.meta.numberOfLines
    ]


renderBody : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> List (Element MarkupMsg)
renderBody count acc settings block =
    case block.heading of
        Paragraph ->
            [ renderParagraphBody count acc settings block ]

        Ordinary name ->
            [ OrdinaryBlock.render count acc settings block ]

        Verbatim name ->
            [ VerbatimBlock.render count acc settings block |> Render.Helper.showError block.meta.error ]


renderParagraphBody : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg
renderParagraphBody count acc settings block =
    case block.body of
        Right exprs ->
            List.map (Render.Expression.render count acc settings) exprs
                |> clickableParagraph block.meta.lineNumber block.meta.numberOfLines (Render.Helper.selectedColor block.meta.id settings)
                |> indentParagraph block.indent

        Left _ ->
            Element.none



---- SUBSIDIARY RENDERERS


clickableParagraph : Int -> Int -> Element.Attribute MarkupMsg -> List (Element MarkupMsg) -> Element MarkupMsg
clickableParagraph lineNumber numberOfLines color elements =
    let
        id =
            String.fromInt lineNumber
    in
    Element.paragraph
        [ color
        , Render.Sync.rightToLeftSyncHelper lineNumber numberOfLines
        , Render.Helper.htmlId id
        ]
        elements


indentParagraph : number -> Element msg -> Element msg
indentParagraph indent x =
    if indent > 0 then
        Element.el [ Element.paddingEach { top = Render.Helper.topPaddingForIndentedElements, bottom = 0, left = 0, right = 0 } ] x

    else
        x
