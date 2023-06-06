module Language exposing
    ( Block(..)
    , BlockMeta
    , Expr(..)
    , ExprMeta
    , Expression
    , ExpressionBlock
    , Heading(..)
    , Name
    , PrimitiveBlock
    , Properties
    , Property(..)
    , SimpleExpressionBlock
    , SimplePrimitiveBlock
    , simplifyBlock
    , simplifyExpr
    , simplifyExpressionBlock
    , simplifyPrimitiveBlock
    )

import Dict exposing (Dict)
import Either exposing (Either(..))



-- PARAMETRIZED TYPES


type Expr metaData
    = Fun String (List String) metaData
    | VFun String String metaData
    | Text String metaData


{-|

    PrimitiveBlocks, content = String
    ExpressionBlocks, content = Either String (List Expression)

-}
type Block content blockMetaData
    = Block
        { heading : Heading
        , indent : Int
        , content : content
        , meta : blockMetaData
        }



-- HEADINGS


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



-- METADATA TYPES


type alias ExprMeta =
    { begin : Int, end : Int, index : Int, id : String }


type alias BlockMeta =
    { lineNumber : Int
    , numberOfLines : Int
    , id : String
    , messages : List String
    , sourceText : String
    , error : Maybe String
    }



-- CONCRETE TYPES


type alias Expression =
    Expr ExprMeta


{-| A block whose content is a list of expressions.
-}
type alias ExpressionBlock =
    Block (Either String (List Expression)) BlockMeta


{-| A block whose content is a String.
-}
type alias PrimitiveBlock =
    Block String BlockMeta



-- SIMPLIFIED TYPES


type alias SimpleExpressionBlock =
    Block (Either String (List (Expr (Maybe ())))) (Maybe ())


type alias SimplePrimitiveBlock =
    Block String (Maybe ())



-- GENERIC SIMPLIFIERS


simplifyBlock : (contentA -> contentB) -> Block contentA blockMeta -> Block contentB (Maybe a)
simplifyBlock simplifyContent (Block block) =
    Block
        { heading = block.heading
        , indent = block.indent
        , content = simplifyContent block.content
        , meta = Nothing
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



-- CONCRETE SIMPLIFIERS


simplifyExpressionBlock : ExpressionBlock -> SimpleExpressionBlock
simplifyExpressionBlock block =
    let
        simplifyContent : Either String (List (Expr exprMeta)) -> Either String (List (Expr (Maybe ())))
        simplifyContent content =
            case content of
                Left str ->
                    Left str

                Right exprs ->
                    Right (List.map simplifyExpr exprs)
    in
    simplifyBlock simplifyContent block


simplifyPrimitiveBlock : PrimitiveBlock -> SimplePrimitiveBlock
simplifyPrimitiveBlock block =
    simplifyBlock identity block
