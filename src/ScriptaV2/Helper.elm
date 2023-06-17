module ScriptaV2.Helper exposing
    ( banner
    , getName
    , setName
    , makeSettings
    , title
    , viewToc
    )

import Generic.ASTTools
import Generic.Language
import Render.Block
import Render.Settings
import Render.TOC

type alias RenderSettings = Render.Settings.RenderSettings


banner =
    Generic.ASTTools.banner


getName =
    Generic.Language.getName

makeSettings =
    Render.Settings.makeSettings


renderBody =
    Render.Block.renderBody


setName =
    Generic.Language.setName


title =
    Generic.ASTTools.title


viewToc counter acc attr ast = Render.TOC.view counter acc attr ast