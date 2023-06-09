module ExpressionParserTests exposing (suite)

import Expect
import Generic.Language
    exposing
        ( Expr(..)
        , simplifyExpr
        )
import M.Expression exposing (parse)
import Test exposing (Test, describe, test)


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
