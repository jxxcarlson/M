module Render.OrdinaryBlock exposing (getAttributes, render)

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
import Render.VerbatimBlock as VerbatimBlock
import String.Extra
import Tools.Utility as Utility


render count acc settings attr block =
    case block.body of
        Left _ ->
            Element.none

        Right _ ->
            case block.heading of
                Ordinary functionName ->
                    case Dict.get functionName blockDict of
                        Nothing ->
                            env count acc settings attr block
                                |> indentOrdinaryBlock block.indent (String.fromInt block.meta.lineNumber) settings

                        Just f ->
                            f count acc settings attr block
                                |> indentOrdinaryBlock block.indent (String.fromInt block.meta.lineNumber) settings

                _ ->
                    Element.none


getAttributes : String -> List (Element.Attribute MarkupMsg)
getAttributes name =
    case Dict.get name attributeDict of
        Nothing ->
            []

        Just attrs ->
            attrs


attributeDict : Dict String (List (Element.Attribute MarkupMsg))
attributeDict =
    Dict.fromList
        [--( "box", [ Background.color (Element.rgb 0.9 0.9 1.0) ] )
         --, ( "theorem", [ Font.italic ] )
        ]


blockDict : Dict String (Int -> Accumulator -> RenderSettings -> List (Element.Attribute MarkupMsg) -> ExpressionBlock -> Element MarkupMsg)
blockDict =
    Dict.fromList
        [ --( "indent", indented )
          ( "center", centered )
        , ( "box", box )

        --, ( "quotation", quotation )
        --, ( "set-key", \_ _ _ _ -> Element.none )
        --, ( "comment", comment )
        --, ( "q", question ) -- xx
        --, ( "a", answer ) -- xx
        --, ( "document", document )
        --, ( "collection", collection )
        , ( "bibitem", bibitem )
        , ( "section", section ) -- xx
        , ( "subheading", subheading ) -- xx

        --, ( "runninghead_", \_ _ _ _ -> Element.none ) -- DEPRECATED
        --, ( "banner", \_ _ _ _ -> Element.none )
        , ( "title", \c a s b -> title c a s b )

        --, ( "subtitle", \_ _ _ _ -> Element.none )
        --, ( "author", \_ _ _ _ -> Element.none )
        --, ( "date", \_ _ _ _ -> Element.none )
        , ( "contents", \_ _ _ _ _ -> Element.none )

        --, ( "tags", \_ _ _ _ -> Element.none )
        --, ( "type", \_ _ _ _ -> Element.none )
        , ( "env", env_ )
        , ( "item", Render.List.item )
        , ( "desc", Render.List.desc )
        , ( "numbered", Render.List.numbered )

        --, ( "index", index )
        --, ( "endnotes", endnotes )
        , ( "setcounter", \_ _ _ _ _ -> Element.none )
        , ( "shiftandsetcounter", \_ _ _ _ _ -> Element.none )

        --, ( "list", \_ _ _ _ -> Element.none )
        ]


bibitem : Int -> Accumulator -> RenderSettings -> List (Element.Attribute MarkupMsg) -> ExpressionBlock -> Element MarkupMsg
bibitem count acc settings attrs block =
    let
        label =
            List.Extra.getAt 0 block.args |> Maybe.withDefault "(12)" |> (\s -> "[" ++ s ++ "]")
    in
    Element.row ([ Element.alignTop, Render.Utility.idAttributeFromInt block.meta.lineNumber, Render.Utility.vspace 0 settings.topMarginForChildren ] ++ Render.Sync.highlightIfIdIsSelected block.meta.lineNumber block.meta.numberOfLines settings)
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Font.bold
            , Element.width (Element.px 34)
            ]
            (Element.text label)
        , Element.paragraph ([ Element.paddingEach { left = 25, right = 0, top = 0, bottom = 0 }, Render.Sync.rightToLeftSyncHelper block.meta.lineNumber block.meta.numberOfLines ] ++ Render.Sync.highlightIfIdIsSelected block.meta.lineNumber block.meta.numberOfLines settings)
            (Render.Helper.renderWithDefault "bibitem" count acc settings attrs (Generic.Language.getExpressionContent block))
        ]


box : Int -> Accumulator -> RenderSettings -> List (Element.Attribute MarkupMsg) -> ExpressionBlock -> Element MarkupMsg
box count acc settings attr block =
    Element.column [ Element.spacing 12 ]
        [ Element.el [ Font.bold ] (Element.text (blockHeading block))
        , Element.paragraph
            []
            (Render.Helper.renderWithDefault "box" count acc settings attr (Generic.Language.getExpressionContent block))
        ]


centered : Int -> Accumulator -> RenderSettings -> List (Element.Attribute MarkupMsg) -> ExpressionBlock -> Element MarkupMsg
centered count acc settings attr block =
    Element.el
        [ Element.width (Element.px settings.width) ]
        (Element.paragraph [ Element.centerX, Element.width (Element.px (settings.width - 100)) ]
            (Render.Helper.renderWithDefault "indent" count acc settings attr (Generic.Language.getExpressionContent block))
        )



-- subheading : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg


subheading count acc settings attr block =
    Element.link
        (sectionBlockAttributes block settings ([ topPadding 10 ] ++ attr))
        { url = Render.Utility.internalLink (settings.titlePrefix ++ "title")
        , label = Element.paragraph [] (Render.Helper.renderWithDefault "| subheading" count acc settings attr (Generic.Language.getExpressionContent block))
        }


section count acc settings attr block =
    -- level 1 is reserved for titles
    let
        headingLevel =
            case Dict.get "level" block.properties of
                Nothing ->
                    2

                Just n ->
                    String.toFloat n |> Maybe.withDefault 3

        fontSize =
            settings.maxHeadingFontSize / sqrt (headingLevel - toFloat acc.deltaLevel) |> round

        sectionNumber =
            Element.el [ Font.size fontSize ] (Element.text (Render.Helper.blockLabel block.properties ++ ". "))

        exprs =
            Generic.Language.getExpressionContent block
    in
    Element.link
        (sectionBlockAttributes block settings [ topPadding 20, Font.size fontSize ])
        { url = Render.Utility.internalLink (settings.titlePrefix ++ "title")
        , label = Element.paragraph [] (sectionNumber :: renderWithDefaultWithSize 18 "??!!" count acc settings attr exprs)
        }


title count acc settings attr block =
    let
        fontSize =
            settings.titleSize

        exprs =
            Generic.Language.getExpressionContent block
    in
    Element.paragraph [ Font.size fontSize, elementAttribute "id" "title" ] (renderWithDefaultWithSize fontSize "??!!" count acc settings attr exprs)


sectionBlockAttributes : ExpressionBlock -> RenderSettings -> List (Element.Attr () MarkupMsg) -> List (Element.Attr () MarkupMsg)
sectionBlockAttributes block settings attrs =
    [ Render.Utility.makeId (Generic.Language.getExpressionContent block)
    , Render.Utility.idAttribute block.meta.id
    ]
        ++ Render.Sync.highlightIfIdIsSelected block.meta.lineNumber block.meta.numberOfLines settings
        ++ attrs


topPadding : Int -> Element.Attribute msg
topPadding k =
    Element.paddingEach { top = k, bottom = 0, left = 0, right = 0 }



-- renderWithDefaultWithSize : Int -> String -> Int -> Accumulator -> RenderSettings -> List Expression -> List (Element MarkupMsg)


renderWithDefaultWithSize size default count acc settings attr exprs =
    if List.isEmpty exprs then
        [ Element.el ([ Font.color settings.redColor, Font.size size ] ++ attr) (Element.text default) ]

    else
        List.map (Render.Expression.render count acc settings attr) exprs


indentOrdinaryBlock : Int -> String -> RenderSettings -> Element msg -> Element msg
indentOrdinaryBlock indent id settings x =
    if indent > 0 then
        Element.el [ Render.Helper.selectedColor id settings, Element.paddingEach { top = Render.Helper.topPaddingForIndentedElements, bottom = 0, left = 0, right = 0 } ] x

    else
        x


{-|

    Used to render generic LaTeX environments

-}



-- env_ : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg


env_ count acc settings attr block =
    case List.head block.args of
        Nothing ->
            Element.paragraph
                [ Render.Utility.idAttributeFromInt block.meta.lineNumber
                , Font.color settings.redColor
                , Render.Sync.rightToLeftSyncHelper block.meta.lineNumber block.meta.numberOfLines
                ]
                [ Element.text "| env (missing name!)" ]

        Just _ ->
            env count acc settings attr block


{-|

    Used to render generic LaTeX environments

-}



-- env : Int -> Accumulator -> RenderSettings -> ExpressionBlock -> Element MarkupMsg


env count acc settings attr block =
    case block.body of
        Left _ ->
            Element.none

        Right exprs ->
            Element.column ([ Element.spacing 8, Render.Utility.idAttributeFromInt block.meta.lineNumber ] ++ Render.Sync.highlightIfIdIsSelected block.meta.lineNumber block.meta.numberOfLines settings)
                [ Element.el
                    [ Font.bold
                    , Render.Sync.rightToLeftSyncHelper block.meta.lineNumber block.meta.numberOfLines
                    ]
                    (Element.text (blockHeading block))
                , Element.paragraph
                    [ Font.italic
                    , Render.Sync.rightToLeftSyncHelper block.meta.lineNumber block.meta.numberOfLines
                    ]
                    (renderWithDefault2 ("??" ++ (Generic.Language.getNameFromHeading block.heading |> Maybe.withDefault "(name)")) count acc settings attr exprs)
                ]



-- renderWithDefault2 : String -> Int -> Accumulator -> RenderSettings -> List Expression -> List (Element MarkupMsg)


renderWithDefault2 _ count acc settings attr exprs =
    List.map (Render.Expression.render count acc settings attr) exprs


{-|

    Used in function env (ender generic LaTeX environments).
    This function numbers blocks for which there is a "label" property

-}
blockHeading : ExpressionBlock -> String
blockHeading block =
    case Generic.Language.getNameFromHeading block.heading of
        Nothing ->
            ""

        Just name ->
            if List.member name [ "banner_", "banner" ] then
                ""

            else
                (name |> String.Extra.toTitleCase)
                    ++ " "
                    ++ (Dict.get "label" block.properties |> Maybe.withDefault "")
                    ++ " "
                    ++ String.join " " block.args