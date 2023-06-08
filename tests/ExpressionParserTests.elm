module ExpressionParserTests exposing (..)

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
        , simplifyExpr
        )
import M.ExpressionParser exposing (parse)
import Test exposing (..)


p str =
    parse 0 str |> List.map simplifyExpr


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "M"
        [ testF "hello" p "hello" [ Text "hello" () ]
        , testF "This is [b important]" p "This is [b important]" [ Text "This is " (), Fun "b" [ Text " important" () ] () ]
        , testF "I like $a^2 + b^2 = c^2$" p "I like $a^2 + b^2 = c^2$" [ Text "I like " (), VFun "math" "a^2 + b^2 = c^2" () ]
        ]
