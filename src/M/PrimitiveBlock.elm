module M.PrimitiveBlock exposing
    ( empty, parse
    , argsAndProperties, elaborate, eq, length, listLength
    )

{-| The main function is

    parse : Language -> (String -> Bool) -> List String -> List PrimitiveBlock

@docs PrimitiveBlock, empty, parse

-}

-- import MicroLaTeX.Expression.TransformLaTeX

import Dict exposing (Dict)
import List.Extra
import M.Language exposing (BlockMeta, Heading(..), PrimitiveBlock, emptyBlockMeta)
import M.Line as Line exposing (HeadingError(..), Line, isEmpty, isNonEmptyBlank)
import Tools.KV as KV
import Tools.Loop exposing (Step(..), loop)


{-| Parse a list of strings into a list of primitive blocks given a markup
language and a function for determining when a string is the first line
of a verbatim block
-}
parse : String -> List String -> List PrimitiveBlock
parse initialId lines =
    loop (init initialId lines) nextStep
        |> List.map (\block -> finalize block)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    (String.left 2 str == "||")
        || (String.left 3 str == "```")
        || (String.left 2 str == "$$")


length : PrimitiveBlock -> Int
length block =
    List.length block.content


listLength1 : List PrimitiveBlock -> Int
listLength1 blocks =
    (List.map length blocks |> List.sum) + List.length blocks - 1


listLength : List PrimitiveBlock -> Int
listLength blocks =
    case List.Extra.unconsLast blocks of
        Nothing ->
            0

        Just ( lastBlock, _ ) ->
            lastBlock.meta.lineNumber + length lastBlock - 1


eq : PrimitiveBlock -> PrimitiveBlock -> Bool
eq b1 b2 =
    if b1.meta.sourceText /= b2.meta.sourceText then
        False

    else if b1.heading /= b2.heading then
        False

    else
        True


empty : PrimitiveBlock
empty =
    M.Language.primitiveBlockEmpty


type alias State =
    { blocks : List PrimitiveBlock
    , currentBlock : Maybe PrimitiveBlock
    , lines : List String
    , id : String
    , inBlock : Bool
    , indent : Int
    , lineNumber : Int
    , position : Int
    , inVerbatim : Bool
    , isVerbatimLine : String -> Bool
    , count : Int
    , blocksCommitted : Int
    , label : String
    , error : Maybe HeadingError
    }



-- TODO: think about the below


finalize : PrimitiveBlock -> PrimitiveBlock
finalize block =
    let
        content =
            List.reverse block.content

        sourceText =
            -- TODO: maybe this should be set at the Primitive block level
            String.join "\n" content

        oldMeta =
            block.meta

        newMeta =
            { oldMeta | sourceText = sourceText }
    in
    -- TODO: is this correct?
    { block | content = content, meta = newMeta }


updateMeta : (BlockMeta -> BlockMeta) -> PrimitiveBlock -> PrimitiveBlock
updateMeta transformMeta block =
    let
        oldMeta =
            block.meta

        newMeta =
            transformMeta oldMeta
    in
    { block | meta = newMeta }


{-|

    Recall: classify position lineNumber, where position
    is the position of the first charabcter in the source
    and lineNumber is the index of the current line in the source

-}
init : String -> List String -> State
init initialId lines =
    { blocks = []
    , currentBlock = Nothing
    , lines = lines
    , id = initialId
    , indent = 0
    , lineNumber = 0
    , inBlock = False
    , position = 0
    , inVerbatim = False
    , isVerbatimLine = isVerbatimLine
    , count = 0
    , blocksCommitted = 0
    , label = "0, START"
    , error = Nothing
    }


blockFromLine : Line -> Result Line.HeadingError PrimitiveBlock
blockFromLine ({ indent, lineNumber, position, prefix, content } as line) =
    case Line.getHeadingData content of
        Err error ->
            Err error

        Ok { heading, args, properties } ->
            let
                meta =
                    { emptyBlockMeta
                        | lineNumber = lineNumber
                        , position = position
                        , sourceText = ""
                        , numberOfLines = 1
                    }
            in
            Ok
                { heading = heading
                , indent = indent
                , args = args
                , properties = properties
                , content = [ prefix ++ content ]
                , meta = meta
                }



-- |> elaborate line


nextStep : State -> Step State (List PrimitiveBlock)
nextStep state =
    case List.head state.lines of
        Nothing ->
            -- finish up: no more lines to process
            case state.currentBlock of
                Nothing ->
                    Done (List.reverse state.blocks)

                Just block_ ->
                    let
                        block =
                            { block_ | content = dropLast block_.content }

                        blocks =
                            if block.content == [ "" ] then
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                List.reverse state.blocks

                            else
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                List.reverse (block :: state.blocks)
                    in
                    Done blocks

        Just rawLine ->
            let
                newPosition =
                    if rawLine == "" then
                        state.position + 1

                    else
                        state.position + String.length rawLine + 1

                currentLine : Line
                currentLine =
                    -- TODO: the below is wrong
                    Line.classify state.position (state.lineNumber + 1) rawLine

                reportAction state_ currentLine_ =
                    case ( state_.inBlock, isEmpty currentLine_, isNonEmptyBlank currentLine_ ) of
                        ( False, True, _ ) ->
                            String.fromInt state_.lineNumber ++ ": advance" ++ " ++ :: " ++ currentLine_.content

                        ( False, False, True ) ->
                            String.fromInt state_.lineNumber ++ ": advance2 (PASS)" ++ " ++ :: " ++ currentLine_.content

                        ( False, False, False ) ->
                            String.fromInt state_.lineNumber ++ ": createBlock" ++ " ++ :: " ++ currentLine_.content

                        ( True, False, _ ) ->
                            String.fromInt state_.lineNumber ++ ": addCurrentLine2" ++ " ++ :: " ++ currentLine_.content

                        ( True, True, _ ) ->
                            String.fromInt state_.lineNumber ++ ": commitBlock" ++ " ++ :: " ++ currentLine_.content

                --_ =
                --    Debug.log (reportAction state currentLine) 1
            in
            case ( state.inBlock, isEmpty currentLine, isNonEmptyBlank currentLine ) of
                -- (in block, current line is empty, current line is blank but not empty)
                -- not in a block, pass over empty line
                ( False, True, _ ) ->
                    Loop (advance newPosition { state | label = "1, EMPTY" })

                -- not in a block, pass over blank, non-empty line
                ( False, False, True ) ->
                    Loop (advance newPosition { state | label = "2, PASS" })

                -- create a new block: we are not in a block, but
                -- the current line is nonempty and nonblank
                ( False, False, False ) ->
                    Loop (createBlock { state | label = "3, NEW" } currentLine)

                -- A nonempty line was encountered inside a block, so add it
                ( True, False, _ ) ->
                    Loop (addCurrentLine2 { state | label = "4, ADD" } currentLine)

                -- commit the current block: we are in a block and the
                -- current line is empty
                ( True, True, _ ) ->
                    Loop (commitBlock { state | label = "5, COMMIT" } currentLine)


advance : Int -> State -> State
advance newPosition state =
    { state
        | lines = List.drop 1 state.lines
        , lineNumber = state.lineNumber + 1
        , position = newPosition
        , count = state.count + 1
    }


addCurrentLine2 : State -> Line -> State
addCurrentLine2 state currentLine =
    case state.currentBlock of
        Nothing ->
            { state | lines = List.drop 1 state.lines }

        Just block ->
            { state
                | lines = List.drop 1 state.lines
                , lineNumber = state.lineNumber + 1
                , position = state.position + String.length currentLine.content
                , count = state.count + 1
                , currentBlock =
                    Just (addCurrentLine_ currentLine block)
            }


addCurrentLine_ : Line -> PrimitiveBlock -> PrimitiveBlock
addCurrentLine_ ({ prefix, content } as line) block =
    let
        oldMeta =
            block.meta

        newMeta =
            { oldMeta | sourceText = block.meta.sourceText ++ "\n" ++ prefix ++ content }
    in
    { block | content = line.content :: block.content, meta = newMeta }


commitBlock : State -> Line -> State
commitBlock state currentLine =
    case state.currentBlock of
        Nothing ->
            { state
                | lines = List.drop 1 state.lines
                , indent = currentLine.indent
            }

        Just block__ ->
            let
                block_ =
                    updateMeta (\m -> { m | id = state.id ++ "-" ++ String.fromInt state.blocksCommitted }) block__

                block =
                    case block_.heading of
                        Paragraph ->
                            block_

                        Ordinary _ ->
                            { block_ | content = block_.content |> dropLast } |> adjustBlock

                        Verbatim _ ->
                            if List.head block_.content == Just "```" then
                                { block_ | content = List.filter (\l -> l /= "```") block_.content }

                            else
                                { block_ | content = dropLast block_.content }

                ( rCurrentBlock, newBlocks ) =
                    if block.content == [ "" ] then
                        ( Err HENoContent, state.blocks )

                    else
                        ( blockFromLine currentLine, block :: state.blocks )
            in
            case rCurrentBlock of
                Err err ->
                    { state | error = Just err }

                Ok currentBlock ->
                    { state
                        | lines = List.drop 1 state.lines
                        , lineNumber = state.lineNumber + 1
                        , position = state.position + String.length currentLine.content
                        , count = state.count + 1
                        , blocksCommitted = state.blocksCommitted + 1
                        , blocks = newBlocks
                        , inBlock = False
                        , inVerbatim = state.isVerbatimLine currentLine.content
                        , currentBlock = Just currentBlock
                    }


adjustBlock : PrimitiveBlock -> PrimitiveBlock
adjustBlock block =
    let
        name =
            getName block
    in
    if name == Just "section" && block.args == [] then
        { block | args = [ "1" ] }

    else if name == Just "subsection" && block.args == [] then
        { block | args = [ "2" ] }

    else if name == Just "subsubsection" && block.args == [] then
        { block | args = [ "3" ] }

    else
        block


createBlock : State -> Line -> State
createBlock state currentLine =
    let
        blocks =
            case state.currentBlock of
                Nothing ->
                    state.blocks

                -- When creating a new block push the current block onto state.blocks
                -- only if its content is nontrivial (not == [""])
                Just block ->
                    if block.content == [ "" ] then
                        state.blocks

                    else
                        block :: state.blocks

        rNewBlock =
            blockFromLine currentLine
    in
    case rNewBlock of
        Err err ->
            { state | error = Just err }

        Ok newBlock ->
            { state
                | lines = List.drop 1 state.lines
                , lineNumber = state.lineNumber + 1
                , position = state.position + String.length currentLine.content
                , count = state.count + 1
                , indent = currentLine.indent
                , inBlock = True
                , currentBlock = Just newBlock
                , blocks = blocks
            }


argsAndProperties : List String -> ( List String, Dict String String )
argsAndProperties words =
    let
        args =
            KV.cleanArgs words

        namedArgs =
            List.drop (List.length args) words

        properties =
            namedArgs |> KV.prepareList |> KV.prepareKVData
    in
    ( words, properties )


elaborate : Line -> PrimitiveBlock -> PrimitiveBlock
elaborate line pb =
    -- TODO: is this function still needed?
    if pb.content == [ "" ] then
        pb

    else
        let
            --( name, args_ ) =
            --    -- TODO: note this change: it needs to be verified
            --    Line.getNameAndArgs lang line
            --
            --( args, properties ) =
            --    argsAndProperties args_
            content =
                case pb.heading of
                    M.Language.Verbatim a ->
                        List.map String.trimLeft pb.content

                    _ ->
                        pb.content

            --if pb.heading == M.Language.Verbatim a then
            --    List.map String.trimLeft pb.content
            --
            --else
            --    pb.content
        in
        { pb
            | content = content

            -- , name = name, args = args, properties = properties
        }


getName : PrimitiveBlock -> Maybe String
getName block =
    case block.heading of
        Paragraph ->
            Nothing

        Ordinary name ->
            Just name

        Verbatim name ->
            Just name


dropLast : List a -> List a
dropLast list =
    let
        n =
            List.length list
    in
    List.take (n - 1) list
