module ExpressionBlockParserTests exposing (..)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Expect exposing (Expectation)
import Generic.ExpressionBlockParser as GEBP
import Generic.Language exposing (BlockMeta, Expr(..), Expression, ExpressionBlock, Heading(..), PrimitiveBlock, SimpleExpressionBlock, SimplePrimitiveBlock, simplifyExpr, simplifyExpressionBlock)
import Test exposing (..)


p str =
    GEBP.toExpressionBlocksFromString "hello\n\n" |> List.map simplifyExpressionBlock


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "parsing to expression blocks"
        [ testF "hello" p "hello\n\n" [ { args = [], body = Right [ Text "hello" () ], firstLine = "hello", heading = Paragraph, indent = 0, meta = (), properties = Dict.fromList [] } ]
        ]
