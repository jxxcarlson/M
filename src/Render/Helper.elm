module Render.Helper exposing (renderWithDefault)

import Element exposing (Element)
import Element.Font as Font
import Generic.Acc
import Generic.Language exposing (Expression)
import Render.Expression
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (RenderSettings)


renderWithDefault : String -> Int -> Generic.Acc.Accumulator -> RenderSettings -> List Expression -> List (Element MarkupMsg)
renderWithDefault default count acc settings exprs =
    if List.isEmpty exprs then
        [ Element.el [ Font.color settings.redColor, Font.size 14 ] (Element.text default) ]

    else
        List.map (Render.Expression.render count acc settings) exprs
