module Render.Math exposing
    ( DisplayMode(..)
    , aligned
    , displayedMath
    , equation
    , mathText
    )

import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Font as Font
import Generic.Acc exposing (Accumulator)
import Generic.Language exposing (ExpressionBlock)
import Generic.MathMacro
import Generic.PTextMacro
import Generic.TextMacro
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (RenderSettings)
import Render.Sync
import Render.Utility


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


leftPadding =
    Element.paddingEach { left = 0, right = 0, top = 0, bottom = 0 }


displayedMath : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg
displayedMath count acc settings block =
    let
        w =
            String.fromInt settings.width ++ "px"

        filteredLines =
            -- lines of math text to be rendered: filter stuff out
            String.lines (getContent block)
                |> List.filter (\line -> not (String.left 2 (String.trim line) == "$$"))
                |> List.filter (\line -> not (String.left 6 line == "[label"))
                |> List.filter (\line -> line /= "")
                |> List.map (Generic.MathMacro.evalStr acc.mathMacroDict)
    in
    Element.column (Render.Sync.rightLeftSyncHelper block.meta.lineNumber block.meta.numberOfLines :: [])
        [ Element.el (Render.Sync.highlighter block.args [ Element.centerX ])
            (mathText count w block.meta.id DisplayMathMode (filteredLines |> String.join "\n"))
        ]


getContent : ExpressionBlock -> String
getContent { body } =
    case body of
        Left str ->
            str

        Right _ ->
            ""


equation : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg
equation count acc settings block =
    let
        w =
            String.fromInt settings.width ++ "px"

        filteredLines =
            -- lines of math text to be rendered: filter stuff out
            String.lines (getContent block)
                |> List.filter (\line -> not (String.left 2 line == "$$") && not (String.left 6 line == "[label") && not (line == "end"))
                |> List.map (Generic.MathMacro.evalStr acc.mathMacroDict)

        content =
            String.join "\n" filteredLines

        labelText =
            "(" ++ (Dict.get "equation-number" block.properties |> Maybe.withDefault "-") ++ ")"

        label_ =
            Element.el [ Font.size 12, Element.alignRight, Element.moveDown 35 ] (Element.text labelText)

        label =
            showIf settings content label_
    in
    Element.column [ Element.width (Element.px settings.width) ]
        [ Element.row
            ([ Element.centerX, Element.spacing 12, Element.inFront label ]
                ++ (Render.Sync.rightLeftSyncHelper block.meta.lineNumber block.meta.numberOfLines
                        :: [ Render.Utility.elementAttribute "id" block.meta.id ]
                   )
            )
            [ Element.el
                (Render.Sync.highlightIfIdSelected block.meta.id
                    settings
                    (Render.Sync.highlighter block.args
                        []
                    )
                )
                (mathText count w block.meta.id DisplayMathMode content)
            ]
        ]


showIf : Render.Settings.RenderSettings -> String -> Element msg -> Element msg
showIf settings content element =
    if Render.Utility.textWidth settings.display content > (toFloat settings.width - 40) then
        Element.none

    else
        element


getCounter : String -> Dict String Int -> String
getCounter counterName dict =
    Dict.get counterName dict |> Maybe.withDefault 0 |> String.fromInt


getLabel : String -> Dict String String -> String
getLabel label dict =
    Dict.get label dict |> Maybe.withDefault "" |> Debug.log "LABEL" |> String.trim


aligned : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg
aligned count acc settings block =
    let
        str =
            case block.body of
                Left str_ ->
                    str_

                Right _ ->
                    ""

        filteredLines =
            -- filter stuff out of lines of math text to be rendered:
            String.lines str
                |> List.filter (\line -> not (String.left 6 line == "[label") && not (line == ""))

        deleteTrailingSlashes str_ =
            if String.right 2 str_ == "\\\\" then
                String.dropRight 2 str_

            else
                str_

        adjustedLines_ =
            List.map (deleteTrailingSlashes >> Generic.MathMacro.evalStr acc.mathMacroDict) filteredLines
                |> List.filter (\line -> line /= "")
                |> List.map (\line -> line ++ "\\\\")

        adjustedLines =
            "\\begin{aligned}" :: adjustedLines_ ++ [ "\\end{aligned}" ]

        content =
            String.join "\n" adjustedLines

        labelText =
            "(" ++ (Dict.get "equation-number" block.properties |> Maybe.withDefault "-") ++ ")"

        label_ =
            Element.el [ Font.size 12, Element.alignRight, Element.moveDown 35 ] (Element.text labelText)

        label =
            showIf settings content label_
    in
    Element.column [ Element.width (Element.px settings.width) ]
        [ Element.row
            ([ Element.centerX, Element.spacing 12, Element.inFront label ]
                ++ (Render.Sync.rightLeftSyncHelper block.meta.lineNumber block.meta.numberOfLines
                        :: [ Render.Utility.elementAttribute "id" block.meta.id ]
                   )
            )
            [ Element.el
                (Render.Sync.highlightIfIdSelected block.meta.id
                    settings
                    (Render.Sync.highlighter block.args
                        []
                    )
                )
                (mathText count str block.meta.id DisplayMathMode content)
            ]
        ]


mathText : Int -> String -> String -> DisplayMode -> String -> Element msg
mathText generation width id displayMode content =
    -- TODO Track this down at the source.
    Html.Keyed.node "span"
        [ HA.style "padding-top" "14px"
        , HA.style "padding-bottom" "14px"
        , HA.id id
        , HA.style "width" width
        ]
        [ ( String.fromInt generation, mathText_ displayMode (eraseLabeMacro content) )
        ]
        |> Element.html


eraseLabeMacro content =
    content |> String.lines |> List.map (Generic.PTextMacro.eraseLeadingMacro "label") |> String.join "\n"


mathText_ : DisplayMode -> String -> Html msg
mathText_ displayMode content =
    Html.node "math-text"
        -- active meta selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker meta
        -- , HA.id (makeId meta)
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True
