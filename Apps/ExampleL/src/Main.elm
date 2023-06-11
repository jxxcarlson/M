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


initialText0 =
    """
\\begin{equation}
a^2 + b^2 = c^2
\\end{equation}


\\begin{theorem}
foo


aaa

$x^2$

"""


initialText2 =
    """

Pythagoras says: $a^2 + b^2 = c^2$

| theorem (Whatever)
There are infinitely many primes

This \\strong{will} be on the test:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

|| image width:fill caption: Yellow bird
https://natureconservancy-h.assetsadobe.com/is/image/content/dam/tnc/nature/en/photos/AmericanGoldfinch_MattWilliams_4000x2200.jpg?crop=0%2C0%2C4000%2C2200&wid=4000&hei=2200&scl=1.0

"""


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
    (appWidth // 2) - 30


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 18, width (px appWidth), height (px 650) ]
            [ -- title "Compiler Demo"
              row [ spacing 18 ]
                [ inputText model
                , displayRenderedText model |> Element.map Render
                ]
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


displayRenderedText model =
    column [ spacing 8, Font.size 14 ]
        [ el [ fontGray 0.9 ] (text "Rendered Text")
        , column
            [ spacing 18
            , Background.color (Element.rgb 1.0 1.0 1.0)
            , width (px panelWidth)
            , height (px 600)
            , paddingXY 16 32
            , scrollbarY
            ]
            (Compiler.compileL "ID" 0 Generic.Compiler.defaultRenderData (String.lines model.sourceText))
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
