module Render.Helper exposing
    ( blockLabel
    , htmlId
    , noSuchOrdinaryBlock
    , noSuchVerbatimBlock
    , nonExportableVerbatimBlocks
    , renderNothing
    , renderWithDefault
    , selectedColor
    , showError
    , topPaddingForIndentedElements
    )

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Generic.Acc exposing (Accumulator)
import Generic.Language exposing (Expr(..), Expression, ExpressionBlock, Heading(..))
import Html.Attributes
import Render.Expression
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (RenderSettings)



-- SETTINGS


topPaddingForIndentedElements =
    10


nonExportableVerbatimBlocks =
    [ "hide", "svg", "chart", "include", "iframe" ]



-- HELPERS


{-|

    Used in function env (render generic LaTeX environments)

-}
blockLabel : Dict String String -> String
blockLabel properties =
    Dict.get "label" properties |> Maybe.withDefault ""


selectedColor id settings =
    if id == settings.selectedId then
        Background.color (Element.rgb 0.9 0.9 1.0)

    else
        Background.color settings.backgroundColor


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)


showError : Maybe String -> Element msg -> Element msg
showError maybeError x =
    case maybeError of
        Nothing ->
            x

        Just error ->
            Element.column []
                [ x
                , Element.el [ Font.color (Element.rgb 0.7 0 0) ] (Element.text error)
                ]



-- ERRORS.


noSuchVerbatimBlock : String -> String -> Element MarkupMsg
noSuchVerbatimBlock functionName content =
    Element.column [ Element.spacing 4 ]
        [ Element.paragraph [ Font.color (Element.rgb255 180 0 0) ] [ Element.text <| "No such block: " ++ functionName ]
        , Element.column [ Element.spacing 4 ] (List.map (\t -> Element.el [] (Element.text t)) (String.lines content))
        ]


noSuchOrdinaryBlock : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg
noSuchOrdinaryBlock count acc settings block =
    Element.column [ Element.spacing 4 ]
        [ Element.paragraph [ Font.color (Element.rgb255 180 0 0) ] [ Element.text <| "No such block:" ++ (block.args |> String.join " ") ]

        -- TODO fix this
        --, Element.paragraph [] (List.map (Render.Expression.render count acc settings) (Generic.Language.getExpressionContent block))
        ]


renderNothing : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg
renderNothing _ _ _ _ =
    Element.none


renderWithDefault : String -> Int -> Generic.Acc.Accumulator -> RenderSettings -> List (Element.Attribute MarkupMsg) -> List Expression -> List (Element MarkupMsg)
renderWithDefault default count acc settings attr exprs =
    if List.isEmpty exprs then
        [ Element.el [ Font.color settings.redColor, Font.size 14 ] (Element.text default) ]

    else
        List.map (Render.Expression.render count acc settings attr) exprs
