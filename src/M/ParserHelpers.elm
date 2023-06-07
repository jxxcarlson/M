module M.ParserHelpers exposing
    ( Step(..)
    , getFirstOccurrence
    , loop
    , prependMessage
    )


getFirstOccurrence : (a -> Bool) -> List a -> Maybe a
getFirstOccurrence predicate list =
    loop list (nextStep predicate)


nextStep : (a -> Bool) -> List a -> Step (List a) (Maybe a)
nextStep predicate list =
    case List.head list of
        Nothing ->
            Done Nothing

        Just item ->
            if predicate item then
                Done (Just item)

            else
                Loop (List.drop 1 list)


prependMessage : Int -> String -> List String -> List String
prependMessage lineNumber message messages =
    (message ++ " (line " ++ String.fromInt lineNumber ++ ")") :: List.take 2 messages
