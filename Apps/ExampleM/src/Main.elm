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
    }


type Msg
    = NoOp
    | InputText String
    | Render MarkupMsg


type alias Flags =
    {}


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


appWidth : Int
appWidth =
    1200


panelWidth : Int
panelWidth =
    ((appWidth - 300) // 2) - 30


mainColumn : Model -> Element Msg
mainColumn model =
    let
        compiled =
            Compiler.compileM panelWidth model.count "@@" (String.lines model.sourceText)
    in
    column mainColumnStyle
        [ column [ spacing 18, width (px appWidth), height (px 650) ]
            [ -- title "Compiler Demo"
              row [ spacing 18 ]
                [ inputText model
                , displayRenderedText compiled.body |> Element.map Render
                , viewToc compiled.toc |> Element.map Render
                ]
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


displayRenderedText compiledBody =
    column [ spacing 8, Font.size 14 ]
        [ el [ fontGray 0.9 ] (text "Rendered Text")
        , column
            [ spacing 18
            , Background.color (Element.rgb 1.0 1.0 1.0)
            , width (px panelWidth)
            , height (px 600)
            , paddingXY 16 32
            , htmlId "rendered-text"
            , scrollbarY
            ]
            compiledBody
        ]


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)


viewToc compiledTOC =
    column [ spacing 8, Font.size 14 ]
        [ el [ fontGray 0.9 ] (text "Table of contents")
        , column
            [ spacing 18
            , Background.color (Element.rgb 1.0 1.0 1.0)
            , width (px 300)
            , height (px 600)
            , paddingXY 16 32
            , scrollbarY
            ]
            compiledTOC
        ]


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px panelWidth), height (px 600), Font.size 14 ]
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
