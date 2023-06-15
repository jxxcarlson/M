module Ex exposing (..)


input =
    [ { args = []
      , body = ()
      , firstLine = "\\begin{theorem}"
      , heading = Ordinary "theorem"
      , indent = 0
      , meta = ()
      , properties = Dict.fromList [ ( "status", "finished" ) ]
      }
    , { args = []
      , body = ()
      , firstLine = "\\begin{equation}"
      , heading = Verbatim "equation"
      , indent = 1
      , meta = ()
      , properties = Dict.fromList [ ( "status", "finished" ) ]
      }
    , { args = []
      , body = ()
      , firstLine = "Wow! Great theorem!!"
      , heading = Paragraph
      , indent = 1
      , meta = ()
      , properties = Dict.fromList [ ( "status", "finished" ) ]
      }
    ]


output =
    Ok
        [ Tree
            { args = []
            , body = ()
            , firstLine = "\\begin{theorem}"
            , heading = Ordinary "theorem"
            , indent = 0
            , meta = ()
            , properties = Dict.fromList [ ( "status", "finished" ) ]
            }
            [ Tree
                { args = []
                , body = ()
                , firstLine = "\\begin{equation}"
                , heading = Verbatim "equation"
                , indent = 1
                , meta = ()
                , properties = Dict.fromList [ ( "status", "finished" ) ]
                }
                []
            , Tree
                { args = []
                , body = ()
                , firstLine = "Wow! Great theorem!!"
                , heading = Paragraph
                , indent = 1
                , meta = ()
                , properties = Dict.fromList [ ( "status", "finished" ) ]
                }
                []
            ]
        ]
