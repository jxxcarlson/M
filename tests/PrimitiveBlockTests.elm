module PrimitiveBlockTests exposing (..)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Expect exposing (Expectation)
import M.Language
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
import M.PrimitiveBlock
import Test exposing (..)


testF : String -> (a -> b) -> a -> b -> Test
testF label f input output =
    test label <|
        \_ -> input |> f |> Expect.equal output


suite : Test
suite =
    describe "PrimitiveBlock parser"
        [ testF "text" (M.PrimitiveBlock.parse << String.lines) text1 [ block1, block2 ]
        ]



-- TEST 1


text1 =
    """abc
def

"""


block1 =
    { args = []
    , content = [ "abc", "def" ]
    , heading = Paragraph
    , indent = 0
    , meta =
        { error = Nothing
        , id = ""
        , lineNumber = 0
        , messages = []
        , numberOfLines = 2
        , position = 0
        , sourceText = "abc\ndef"
        }
    , properties = Dict.fromList []
    }


block2 =
    { args = []
    , content = []
    , heading = Paragraph
    , indent = 0
    , meta =
        { error = Nothing
        , id = ""
        , lineNumber = 3
        , messages = []
        , numberOfLines = 1
        , position = 6
        , sourceText = ""
        }
    , properties = Dict.fromList []
    }



-- TEST 2


text2 =
    """abc
def

ghi
jkl

"""


block2b =
    { args = []
    , content = [ "abc", "def" ]
    , heading = Paragraph
    , indent = 0
    , meta =
        { error = Nothing
        , id = ""
        , lineNumber = 1
        , messages = []
        , numberOfLines = 1
        , position = 0
        , sourceText = "abc\ndef"
        }
    , properties = Dict.fromList []
    }
