module LanguageTypeTests exposing (..)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Expect exposing (Expectation)
import Language
    exposing
        ( Block(..)
        , BlockMeta
        , Expr(..)
        , Expression
        , ExpressionBlock
        , Heading(..)
        , PrimitiveBlock
        , SimpleExpressionBlock
        , SimplePrimitiveBlock
        )
import Test exposing (..)


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "M"
        [ testF "simplifyExpr" Language.simplifyExpr expr1 expr2
        , testF "simplifyPrimitiveBlock" Language.simplifyPrimitiveBlock primitiveBlock1 primitiveBlock1Simplified
        , testF "simplifyExpressionBlock" Language.simplifyExpressionBlock expressionBlock expressionBlockSimplified
        ]



-- EXPRESSIONS


expr1 =
    Fun "bold" [ "Hello" ] exprMeta


expr2 =
    Fun "bold" [ "Hello" ] Nothing



-- METADATA


exprMeta =
    { begin = 0, end = 10, index = 3, id = "abc" }


blockMetaExample =
    { lineNumber = 0
    , numberOfLines = 1
    , id = "abc"
    , messages = [ "Hello", "World" ]
    , sourceText = "This is a test."
    , error = Nothing
    }



-- HEADINGS


parHeading : Heading
parHeading =
    Paragraph


ordinaryHeading : Heading
ordinaryHeading =
    Ordinary "quotation" Dict.empty


verbatimHeading : Heading
verbatimHeading =
    Verbatim "verbatim" Dict.empty



-- PRIMITIVE BLOCKS


primitiveBlock1 : PrimitiveBlock
primitiveBlock1 =
    Block
        { heading = parHeading
        , indent = 0
        , content = "This is a test"
        , meta = blockMetaExample
        }


primitiveBlock1Simplified : SimplePrimitiveBlock
primitiveBlock1Simplified =
    Block
        { heading = parHeading
        , indent = 0
        , content = "This is a test"
        , meta = Nothing
        }



-- EXPRESSION BLOCKS


expressionBlock : ExpressionBlock
expressionBlock =
    Block
        { heading = parHeading
        , indent = 0
        , content = Right [ Text "This is a test" exprMeta ]
        , meta = blockMetaExample
        }


expressionBlockSimplified : SimpleExpressionBlock
expressionBlockSimplified =
    Block
        { heading = parHeading
        , indent = 0
        , content = Right [ Text "This is a test" Nothing ]
        , meta = Nothing
        }
