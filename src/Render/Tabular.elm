module Render.Tabular exposing (render)

import Dict exposing (Dict)
import Element exposing (Element)
import Generic.Acc exposing (Accumulator)
import Generic.Language exposing (Expression, ExpressionBlock)
import List.Extra
import M.Expression
import Render.Expression
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Sync
import Render.Utility
import Tools.Utility as Utility


render : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
render count acc settings block =
    let
        formatString : List String
        formatString =
            String.words (List.head block.args |> Maybe.withDefault "")

        formatList : List (Element.Attribute msg)
        formatList =
            List.map (\c -> Dict.get c formatDict |> Maybe.withDefault Element.centerX) formatString

        lines =
            Render.Utility.getVerbatimContent block |> String.split "\\\\"

        cellsAsStrings_ : List (List String)
        cellsAsStrings_ =
            List.map (String.split "&") lines
                |> List.map (List.map String.trim)

        effectiveFontWidth_ =
            9.0

        maxRowSize : Maybe Int
        maxRowSize =
            List.map List.length cellsAsStrings_ |> List.maximum

        cellsAsStrings =
            List.filter (\row_ -> Just (List.length row_) == maxRowSize) cellsAsStrings_

        columnWidths : List Int
        columnWidths =
            List.map (List.map Render.Utility.textWidth) cellsAsStrings
                |> List.Extra.transpose
                |> List.map (\column -> List.maximum column |> Maybe.withDefault 1)
                |> List.map ((\w -> effectiveFontWidth_ * w) >> round)

        fix colWidths fmtList =
            let
                m =
                    List.length colWidths

                n =
                    List.length fmtList
            in
            case compare m n of
                LT ->
                    List.repeat m Element.centerX

                EQ ->
                    fmtList

                GT ->
                    List.repeat m Element.centerX

        extendedFormatList =
            List.map2 (\x y -> ( x, y )) columnWidths (fix columnWidths formatList)

        totalWidth =
            List.sum columnWidths

        parsedCells : List (List (List Expression))
        parsedCells =
            List.map (List.map (M.Expression.parse 0)) cellsAsStrings

        renderer : Expression -> Element MarkupMsg
        renderer =
            Render.Expression.render count acc settings

        tableCell : ( Int, Element.Attribute MarkupMsg ) -> List (Element MarkupMsg) -> Element MarkupMsg
        tableCell ( colWidth, fmt ) list =
            Element.el [ Element.width (Element.px (colWidth + 18)) ]
                (Element.row [ Element.paddingXY 12 8, fmt ] list)

        renderCell : ( Int, Element.Attribute MarkupMsg ) -> List Expression -> Element MarkupMsg
        renderCell ( colWidth, fmt ) =
            List.map renderer >> tableCell ( colWidth, fmt )

        renderRow : List ( Int, Element.Attribute MarkupMsg ) -> List (List Expression) -> Element MarkupMsg
        renderRow formats cells =
            List.map2 renderCell formats cells |> Element.row []

        renderTable : List ( Int, Element.Attribute MarkupMsg ) -> List (List (List Expression)) -> List (Element MarkupMsg)
        renderTable formats cells =
            let
                f : List (List Expression) -> Element MarkupMsg
                f =
                    renderRow formats
            in
            List.map (renderRow formats) cells
    in
    Element.column
        [ Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }
        , Render.Sync.rightLeftSyncHelper block.meta.lineNumber (block.meta.lineNumber + block.meta.numberOfLines)
        , Render.Utility.idAttributeFromInt block.meta.lineNumber
        ]
        (renderTable extendedFormatList parsedCells)


formatDict : Dict String (Element.Attribute msg)
formatDict =
    Dict.fromList
        [ ( "l", Element.alignLeft )
        , ( "r", Element.alignRight )
        , ( "c", Element.centerX )
        ]
