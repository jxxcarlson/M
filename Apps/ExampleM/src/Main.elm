module Main exposing (main)

import Browser
import Compiler
import Data
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Generic.Compiler
import Html exposing (Html)
import Html.Attributes
import Render.Msg exposing (MarkupMsg)
import Render.Settings


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { sourceText : String
    , count : Int
    , windowWidth : Int
    , windowHeight : Int
    }


type Msg
    = NoOp
    | InputText String
    | Render MarkupMsg


type alias Flags =
    { window : { windowWidth : Int, windowHeight : Int } }


displaySettings : Int -> Generic.Compiler.DisplaySettings
displaySettings counter =
    { windowWidth = 500
    , counter = counter
    , longEquationLimit = 100
    , selectedId = "--"
    , selectedSlug = Nothing
    , scale = 0.8
    }


initialText =
    Data.initialText


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sourceText = initialText
      , count = 0
      , windowWidth = flags.window.windowWidth
      , windowHeight = flags.window.windowHeight
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model
                | sourceText = str
                , count = model.count + 1
              }
            , Cmd.none
            )

        Render _ ->
            ( model, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    layoutWith { options = [] }
        [ bgGray 0.2 ]
        (mainColumn model)


appWidth : Model -> Int
appWidth model =
    model.windowWidth


appHeight : Model -> Int
appHeight model =
    model.windowHeight


tocWidth =
    250


panelWidth : Model -> Int
panelWidth model =
    (appWidth model - tocWidth - (margin.left + margin.right + 2 * margin.between)) // 2


panelHeight : Model -> Attribute msg
panelHeight model =
    height (px <| appHeight model - margin.bottom - margin.top)


margin =
    { left = 20, right = 20, top = 20, bottom = 20, between = 20 }


paddingZero =
    { left = 0, right = 0, top = 0, bottom = 0 }


mainColumn : Model -> Element Msg
mainColumn model =
    let
        compiled =
            Compiler.compileM (panelWidth model) model.count "(selectedId)" (String.lines model.sourceText)
    in
    column mainColumnStyle
        [ column [ width (px <| appWidth model), height (px <| appHeight model) ]
            [ -- title "Compiler Demo"
              row [ spacing margin.between, centerX, paddingEach { paddingZero | left = margin.left, right = margin.right } ]
                [ el [ paddingEach { paddingZero | left = margin.left } ] (inputText model)
                , displayRenderedText model compiled.body |> Element.map Render
                , el [ paddingEach { paddingZero | right = margin.right } ] (viewToc model compiled.toc) |> Element.map Render
                ]
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


displayRenderedText model compiledBody =
    column [ spacing 8, Font.size 14 ]
        [ el [ fontGray 0.9 ] (text "Rendered Text")
        , column
            [ spacing 18
            , Background.color (Element.rgb 1.0 1.0 1.0)
            , width (px <| panelWidth model)
            , panelHeight model
            , paddingXY 16 32
            , htmlId "rendered-text"
            , scrollbarY
            ]
            compiledBody
        ]


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)


viewToc model compiledTOC =
    column [ spacing 8, Font.size 14 ]
        [ el [ fontGray 0.9 ] (text "Table of contents")
        , column
            [ spacing 18
            , Background.color (Element.rgb 1.0 1.0 1.0)
            , width (px tocWidth)
            , panelHeight model
            , paddingXY 16 32
            , scrollbarY
            ]
            compiledTOC
        ]


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px <| panelWidth model), panelHeight model, Font.size 14 ]
        { onChange = InputText
        , text = model.sourceText
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Source text")
        , spellcheck = False
        }


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 0.4
    , paddingXY 20 20
    ]
