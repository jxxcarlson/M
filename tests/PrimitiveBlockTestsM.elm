module PrimitiveBlockTestsM exposing (suite)

import Dict
import Expect
import Generic.Language
    exposing
        ( Heading(..)
        )
import M.PrimitiveBlock
import Test exposing (Test, describe, test)


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "PrimitiveBlock parser"
        [ testF "test1" (M.PrimitiveBlock.parse "abc" << String.lines) input1 output1
        , testF "test2" (M.PrimitiveBlock.parse "!!" << String.lines) input2 output2
        ]



-- TEST 1


input1 =
    """abc
def

"""


output1 =
    [ { args = []
      , body = [ "abc", "def" ]
      , firstLine = "abc"
      , heading = Paragraph
      , indent = 0
      , meta =
            { error = Nothing
            , id = "abc-0"
            , lineNumber = 1
            , messages = []
            , numberOfLines = 2
            , position = 0
            , sourceText = "abc\ndef"
            }
      , properties = Dict.fromList []
      }
    ]



-- TEST 2


input2 =
    """abc
def
ghi

123
456
789
       
"""


output2 =
    [ { args = []
      , body = [ "abc", "def", "ghi" ]
      , firstLine = "abc"
      , heading = Paragraph
      , indent = 0
      , meta =
            { error = Nothing
            , id = "!!-0"
            , lineNumber = 1
            , messages = []
            , numberOfLines = 3
            , position = 0
            , sourceText = "abc\ndef\nghi"
            }
      , properties = Dict.fromList []
      }
    , { args = []
      , body = [ "123", "456", "789", "" ]
      , firstLine = "123"
      , heading = Paragraph
      , indent = 0
      , meta =
            { error = Nothing
            , id = "!!-1"
            , lineNumber = 5
            , messages = []
            , numberOfLines = 4
            , position = 13
            , sourceText = "123\n456\n789\n"
            }
      , properties = Dict.fromList []
      }
    ]
