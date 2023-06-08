module ExpressionForestTests exposing (suite)

import Expect exposing (Expectation)
import M.Parser
import Test exposing (Test, describe, test)


p str =
    M.Parser.f str |> Result.map List.length


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "parsing to expression blocks"
        [ testF "hello, 1" p "hello\n\n" (Ok 1)
        , testF "hello, 2" p "hello\n\nthere\n\n" (Ok 2)
        , testF "hello, 2 has child" p "hello\n\n  there\n\n" (Ok 1)
        ]
