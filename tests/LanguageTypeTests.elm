module LanguageTypeTests exposing (suite)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Expect exposing (Expectation)
import Generic.Language
    exposing
        ( BlockMeta
        , Expr(..)
        , Expression
        , ExpressionBlock
        , Heading(..)
        , PrimitiveBlock
        , SimpleExpressionBlock
        , SimplePrimitiveBlock
        )
import Test exposing (Test, describe, test)


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "M"
        [ testF "simplifyExpr" Generic.Language.simplifyExpr expr1 expr2
        , testF "simplifyPrimitiveBlock" Generic.Language.simplifyPrimitiveBlock primitiveBlock1 primitiveBlock1Simplified
        , testF "simplifyExpressionBlock" Generic.Language.simplifyExpressionBlock expressionBlock expressionBlockSimplified
        ]



-- EXPRESSIONS


expr1 =
    Fun "bold" [ Text "Hello" exprMeta ] exprMeta


expr2 =
    Fun "bold" [ Text "Hello" () ] ()



-- METADATA


exprMeta =
    { begin = 0, end = 10, index = 3, id = "abc" }


blockMetaExample =
    { position = 0
    , lineNumber = 0
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
    Ordinary "quotation"


verbatimHeading : Heading
verbatimHeading =
    Verbatim "verbatim"



-- PRIMITIVE BLOCKS


primitiveBlock1 : PrimitiveBlock
primitiveBlock1 =
    { heading = parHeading
    , indent = 0
    , args = []
    , properties = Dict.empty
    , firstLine = ""
    , body = [ "This is a test" ]
    , meta = blockMetaExample
    }


primitiveBlock1Simplified : SimplePrimitiveBlock
primitiveBlock1Simplified =
    { heading = parHeading
    , indent = 0
    , args = []
    , properties = Dict.empty
    , firstLine = ""
    , body = [ "This is a test" ]
    , meta = ()
    }



-- EXPRESSION BLOCKS


expressionBlock : ExpressionBlock
expressionBlock =
    { heading = parHeading
    , indent = 0
    , args = []
    , properties = Dict.empty
    , firstLine = ""
    , body = Right [ Text "This is a test" exprMeta ]
    , meta = blockMetaExample
    }


expressionBlockSimplified : SimpleExpressionBlock
expressionBlockSimplified =
    { heading = parHeading
    , indent = 0
    , args = []
    , properties = Dict.empty
    , firstLine = ""
    , body = Right [ Text "This is a test" () ]
    , meta = ()
    }
