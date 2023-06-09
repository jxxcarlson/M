module ScriptaV2.Compiler exposing
    ( compile
    , compileL
    , compileM
    , compileX
    , parseL
    , parseM
    , parseX
    , pm
    )

--import Render.Block

import Element exposing (Element)
import Generic.ASTTools
import Generic.Acc
import Generic.Compiler
import Generic.Forest exposing (Forest)
import Generic.ForestTransform exposing (Error)
import Generic.Language exposing (ExpressionBlock)
import M.Expression
import M.PrimitiveBlock
import MicroLaTeX.Expression
import MicroLaTeX.PrimitiveBlock
import MicroLaTeX.Util
import Render.Block
import Render.Msg exposing (MarkupMsg(..))
import Render.TOC
import Render.Tree
import ScriptaV2.Config as Config
import ScriptaV2.Language exposing (Language(..))
import XMarkdown.Expression
import XMarkdown.PrimitiveBlock


compile : Language -> Int -> Int -> String -> List String -> CompilerOutput
compile lang width outerCount selectedId lines =
    case lang of
        L0Lang ->
            compileM width outerCount selectedId lines

        MicroLaTeXLang ->
            compileL width outerCount selectedId lines

        XMarkdownLang ->
            compileX width outerCount selectedId lines


{-|

    > cm "hello!\n\n  [b how are you?]\n\n  $x^2 = 7$\n\n"
    Ok [ Tree { args = [], body = Right [Text "hello!" ()], firstLine = "hello!", heading = Paragraph
           , indent = 0, meta = () , properties = Dict.fromList [] }
         [ Tree { args = [], body = Right [Text ("   ") (),Fun "b" [Text (" how are you?") ()] ()]
            , firstLine = "[b how are you?]", heading = Paragraph, indent = 2, meta = ()
            , properties = Dict.fromList [] } []
         , Tree { args = [], body = Right [Text ("   ") (),VFun "math" ("x^2 = 7") ()]
           , firstLine = "$x^2 = 7$", heading = Paragraph, indent = 2, meta = ()
           , properties = Dict.fromList [] } []]]

    -- proof that the output is a one-tree forest
    > cm "hello!\n\n  [b how are you?]\n\n  $x^2 = 7$\n\n" |> Result.map List.length
    Ok 1

-}
pm str =
    parseM "!!" 0 (String.lines str) |> Result.map (Generic.Forest.map Generic.Language.simplifyExpressionBlock)


parseM : String -> Int -> List String -> Result Error (Forest ExpressionBlock)
parseM idPrefix outerCount lines =
    Generic.Compiler.parse_ M.PrimitiveBlock.parse M.Expression.parse idPrefix outerCount lines


parseX : String -> Int -> List String -> Result Error (Forest ExpressionBlock)
parseX idPrefix outerCount lines =
    Generic.Compiler.parse_ XMarkdown.PrimitiveBlock.parse XMarkdown.Expression.parse idPrefix outerCount lines



-- M compiler


type alias CompilerOutput =
    { body : List (Element MarkupMsg)
    , banner : Maybe (Element MarkupMsg)
    , toc : List (Element MarkupMsg)
    }


compileM : Int -> Int -> String -> List String -> CompilerOutput
compileM width outerCount selectedId lines =
    case parseM Config.idPrefix outerCount lines of
        Err err ->
            { body = [ Element.text "Oops something went wrong" ], banner = Nothing, toc = [] }

        Ok forest_ ->
            let
                renderData =
                    Generic.Compiler.defaultRenderData width outerCount selectedId

                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_
            in
            { body =
                List.map (Render.Tree.renderTreeQ renderData.count accumulator renderData.settings []) forest
            , banner = Nothing --Generic.ASTTools.banner forest |> Maybe.map (Render.Block.renderBody renderData.count accumulator renderData.settings [])
            , toc =
                Render.TOC.view renderData.count accumulator [] forest
            }



-- makeSettings id selectedSlug scale windowWidth


compileX : Int -> Int -> String -> List String -> CompilerOutput
compileX width outerCount selectedId lines =
    case parseX Config.idPrefix outerCount lines of
        Err err ->
            { body = [ Element.text "Oops something went wrong" ], banner = Nothing, toc = [] }

        Ok forest_ ->
            let
                renderData =
                    Generic.Compiler.defaultRenderData width outerCount selectedId

                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_
            in
            { body =
                List.map (Render.Tree.renderTreeQ renderData.count accumulator renderData.settings []) forest
            , banner = Nothing --Generic.ASTTools.banner forest |> Maybe.map (Render.Block.renderBody renderData.count accumulator renderData.settings [])
            , toc =
                Render.TOC.view renderData.count accumulator [] forest
            }



-- LaTeX compiler


{-|

    > pl str = parseL "!!" (String.lines str) |> Result.map (F.map simplifyExpressionBlock)

-}
parseL : String -> Int -> List String -> Result Error (Forest ExpressionBlock)
parseL idPrefix outerCount lines =
    Generic.Compiler.parse_ MicroLaTeX.PrimitiveBlock.parse MicroLaTeX.Expression.parse idPrefix outerCount lines


compileL : Int -> Int -> String -> List String -> CompilerOutput
compileL width outerCount selectedId lines =
    case parseL Config.idPrefix outerCount lines of
        Err err ->
            { body = [ Element.text "Oops something went wrong" ], banner = Nothing, toc = [] }

        Ok forest_ ->
            let
                renderData =
                    Generic.Compiler.defaultRenderData width outerCount selectedId

                ( accumulator, forest ) =
                    Generic.Acc.transformAccumulate renderData.initialAccumulatorData forest_
            in
            { body =
                List.map (Render.Tree.renderTreeQ renderData.count accumulator renderData.settings []) forest
            , banner = Nothing -- Generic.ASTTools.banner forest |> Maybe.map (Render.Block.renderBody renderData.count accumulator renderData.settings [])
            , toc =
                Render.TOC.view renderData.count accumulator [] forest
            }



--
