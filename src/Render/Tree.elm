module Render.Tree exposing (render, unravel)

import Element exposing (Element)
import Element.Font as Font
import Generic.Acc exposing (Accumulator)
import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (ExpressionBlock)
import Generic.Pipeline
import Generic.Settings
import M.ExpressionParser
import Render.Block
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Tree exposing (Tree)


{-| Transform a tree of expression blocks to Element MarkupMsg ("HTML")
-}
render : Int -> Accumulator -> Settings -> Tree ExpressionBlock -> Element MarkupMsg
render count accumulator settings tree =
    let
        blockName =
            Generic.Language.getName (Tree.label tree)
                |> Maybe.withDefault "---"
    in
    if List.member blockName Generic.Settings.numberedBlockNames then
        Element.el [ Font.italic ] ((Tree.map (Render.Block.render count accumulator settings) >> unravel) tree)

    else
        (Tree.map (Render.Block.render count accumulator settings) >> unravel) tree


unravel : Tree (Element MarkupMsg) -> Element MarkupMsg
unravel tree =
    let
        children =
            Tree.children tree
    in
    if List.isEmpty children then
        Tree.label tree

    else
        Element.column []
            --  Render.Settings.leftIndentation,
            [ Tree.label tree
            , Element.column [ Element.paddingEach { top = Render.Settings.defaultSettings.topMarginForChildren, left = Render.Settings.defaultSettings.leftIndent, right = 0, bottom = 0 } ] (List.map unravel children)
            ]
