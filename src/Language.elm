module Language exposing (..)

import Dict exposing (Dict)
import Either exposing (Either(..))


type Expr meta
    = Fun String (List String) meta
    | VFun String String meta
    | Text String meta


type Block blockMeta exprMeta
    = Block
        { heading : Heading
        , indent : Int
        , content : Either String (List (Expr exprMeta))
        , meta : blockMeta
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
