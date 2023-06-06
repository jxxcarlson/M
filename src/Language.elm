module Language exposing
    ( Block(..)
    , BlockMeta
    , Expr(..)
    , ExprBlock
    , ExprMeta
    , Heading(..)
    , Name
    , PrimitiveBlock
    , Properties
    , Property(..)
    , simplifyBlock
    , simplifyExpr
    )

import Dict exposing (Dict)
import Either exposing (Either(..))


type Expr meta
    = Fun String (List String) meta
    | VFun String String meta
    | Text String meta


{-| A block whose content is a list of expressions.
-}
type alias ExprBlock exprMeta blockMeta =
    Block blockMeta (Either String (List (Expr exprMeta)))


{-| A block whose content is a String.
-}
type alias PrimitiveBlock blockMeta =
    Block blockMeta String


type Block content blockMeta
    = Block
        { heading : Heading
        , indent : Int
        , content : content
        , meta : blockMeta
        }


simplifyBlock : Block content blockMeta -> Block content (Maybe a)
simplifyBlock (Block block) =
    Block
        { heading = block.heading
        , indent = block.indent
        , content = block.content
        , meta = Nothing
        }


type Heading
    = Paragraph
    | Ordinary Name Properties
    | Verbatim Name Properties


type alias Name =
    String


type alias Properties =
    Dict String Property


type Property
    = I Int
    | S String
    | B Bool


type alias ExprMeta =
    { begin : Int, end : Int, index : Int, id : String }


type alias BlockMeta =
    { lineNumber : Int
    , numberOfLnes : Int
    , id : String
    , messages : List String
    , sourceText : String
    , error : Maybe String
    }


simplifyExpr : Expr meta -> Expr (Maybe a)
simplifyExpr expr =
    case expr of
        Fun name args _ ->
            Fun name args Nothing

        VFun name arg _ ->
            VFun name arg Nothing

        Text text _ ->
            Text text Nothing
