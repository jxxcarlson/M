module ExpressionBlockParserTests exposing (suite)

import Dict
import Either exposing (Either(..))
import Expect
import Generic.Language exposing (Expr(..), Heading(..), simplifyExpressionBlock)
import M.Parser
import Test exposing (Test, describe, test)


p str =
    M.Parser.toExpressionBlocksFromString 0 str |> List.map simplifyExpressionBlock


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "parsing to expression blocks"
        [ testF "hello" p "hello\n\n" [ { args = [], body = Right [ Text "hello" () ], firstLine = "hello", heading = Paragraph, indent = 0, meta = (), properties = Dict.fromList [] } ]
        ]
