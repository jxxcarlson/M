module ScriptaV2.Language exposing (ExpressionBlock,
  RenderSettings,
  Language(..))

import Generic.Language
import Render.Settings

type Language
    = MicroLaTeXLang
    | L0Lang
    | XMarkdownLang

type alias RenderSettings =
    Render.Settings.RenderSettings

type alias ExpressionBlock =
    Generic.Language.ExpressionBlock
