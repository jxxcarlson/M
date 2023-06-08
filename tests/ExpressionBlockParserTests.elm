module ExpressionBlockParserTests exposing (..)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Expect exposing (Expectation)
import Generic.Language exposing (BlockMeta, Expr(..), Expression, ExpressionBlock, Heading(..), PrimitiveBlock, SimpleExpressionBlock, SimplePrimitiveBlock, simplifyExpr, simplifyExpressionBlock)
import Generic.Pipeline as Pipeline
import Test exposing (..)


p str =
    Pipeline.toExpressionBlocksFromString str |> List.map simplifyExpressionBlock


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "parsing to expression blocks"
        [ testF "hello" p "hello\n\n" [ { args = [], body = Right [ Text "hello" () ], firstLine = "hello", heading = Paragraph, indent = 0, meta = (), properties = Dict.fromList [] } ]
        ]
