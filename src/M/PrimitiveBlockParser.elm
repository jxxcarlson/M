module M.PrimitiveBlock exposing
    ( empty, parse
    , argsAndProperties, eq, length, listLength
    )

{-| The main function is

    parse : String -> List String -> List PrimitiveBlock

@docs PrimitiveBlock, empty, parse

-}

-- import MicroLaTeX.Expression.TransformLaTeX

import Dict exposing (Dict)
import List.Extra
import M.Language exposing (BlockMeta, Heading(..), PrimitiveBlock, emptyBlockMeta)
import M.Line as Line exposing (HeadingError(..), Line)
import M.Regex
import Tools.KV as KV
import Tools.Loop exposing (Step(..), loop)


{-| Parse a list of strings into a list of primitive blocks given
a function for determining when a string is the first line
of a verbatim block

NOTE (TODO) for the moment we assume that the input ends with
a blank line.

-}
parse : String -> List String -> List PrimitiveBlock
parse initialId lines =
    loop (init initialId lines) nextStep


type alias State =
    { blocks : List PrimitiveBlock
    , currentBlock : Maybe PrimitiveBlock
    , lines : List String -- the input
    , idPrefix : String -- the prefix used for block ids
    , inBlock : Bool
    , indent : Int
    , lineNumber : Int
    , position : Int -- the string position in the input text of the first character in the block (an "offset")
    , inVerbatim : Bool
    , isVerbatimLine : String -> Bool
    , count : Int
    , blocksCommitted : Int
    , label : String
    , error : Maybe HeadingError
    }


isVerbatimLine : String -> Bool
isVerbatimLine str =
    (String.left 2 str == "||")
        || (String.left 3 str == "```")
        || (String.left 2 str == "$$")


length : PrimitiveBlock -> Int
length block =
    List.length block.body


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


{-|

    Reverse the order of the strings in the body.
    Then prepend the first line, and concatenate the result.
    This is the source text of the block.

-}
finalize : PrimitiveBlock -> PrimitiveBlock
finalize block =
    let
        content =
            List.reverse block.body

        sourceText =
            if block.heading /= Paragraph then
                String.join "\n" (block.firstLine :: content)

            else
                String.join "\n" content

        oldMeta =
            block.meta

        newMeta =
            { oldMeta | sourceText = sourceText }
    in
    { block | body = content, meta = newMeta }


updateMeta : (BlockMeta -> BlockMeta) -> PrimitiveBlock -> PrimitiveBlock
updateMeta transformMeta block =
    let
        oldMeta =
            block.meta

        newMeta =
            transformMeta oldMeta
    in
    { block | meta = newMeta }



--updateSubRecord : (record -> a) -> (a -> a) -> record -> record
--updateSubRecord fieldF transformSubrecord record =
--    let
--        oldSubRecord =
--            fieldF record
--
--        newSubRecord =
--            transformSubrecord oldSubRecord
--    in
--    { record | meta = newSubRecord }


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
    , idPrefix = initialId
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
                , firstLine = content
                , body = [ prefix ++ content ]
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
                            { block_ | body = dropLast block_.body }

                        blocks =
                            if block.body == [ "" ] then
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
                    state.position + String.length rawLine + 1

                currentLine : Line
                currentLine =
                    -- TODO: the below is wrong
                    Line.classify state.position (state.lineNumber + 1) rawLine
            in
            case ( state.inBlock, Line.isEmpty currentLine, Line.isNonEmptyBlank currentLine ) of
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
                    Loop (createBlock { state | position = newPosition, label = "3, NEW" } currentLine)

                -- A nonempty line was encountered inside a block, so add it
                ( True, False, _ ) ->
                    Loop (addCurrentLine2 { state | position = newPosition, label = "4, ADD" } currentLine)

                -- commit the current block: we are in a block and the
                -- current line is empty
                ( True, True, _ ) ->
                    Loop (commitBlock { state | position = newPosition, label = "5, COMMIT" } currentLine)


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
    { block | body = line.content :: block.body, meta = newMeta }


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
                    block__
                        |> updateMeta (\m -> { m | id = state.idPrefix ++ "-" ++ String.fromInt state.blocksCommitted })
                        |> updateMeta (\m -> { m | numberOfLines = List.length block__.body })

                block =
                    case block_.heading of
                        Paragraph ->
                            block_ |> finalize

                        Ordinary _ ->
                            { block_ | body = block_.body |> dropLast } |> finalize |> transformBlock

                        Verbatim _ ->
                            if List.head block_.body == Just "```" then
                                { block_ | body = List.filter (\l -> l /= "```") block_.body }
                                    |> finalize

                            else
                                { block_ | body = dropLast block_.body } |> finalize
            in
            { state
                | lines = List.drop 1 state.lines
                , lineNumber = state.lineNumber + 1
                , count = state.count + 1
                , blocksCommitted = state.blocksCommitted + 1
                , blocks = block :: state.blocks
                , inBlock = False
                , inVerbatim = state.isVerbatimLine currentLine.content
                , currentBlock = Nothing
            }


fixMarkdownTitleBlock : PrimitiveBlock -> PrimitiveBlock
fixMarkdownTitleBlock block =
    case M.Regex.findTitlePrefix block.firstLine of
        Nothing ->
            block

        Just prefix ->
            { block | body = String.replace prefix "" block.firstLine :: block.body }


{-|

    transformBlock provides for certain notational conveniences, e.g.:

       - write "| section" instead of "| section\n1"
        - write "| subsection" instead of "| section\n2"

-}
transformBlock : PrimitiveBlock -> PrimitiveBlock
transformBlock block =
    case getName block of
        Just "section" ->
            let
                fixedBlock =
                    fixMarkdownTitleBlock block
            in
            case List.head block.args of
                Nothing ->
                    { fixedBlock | properties = Dict.insert "level" "1" block.properties }

                Just level ->
                    { fixedBlock | properties = Dict.insert "level" level block.properties }

        Just "subsection" ->
            { block | properties = Dict.insert "level" "2" block.properties, heading = Ordinary "section" }

        Just "subsubsection" ->
            { block | properties = Dict.insert "level" "3" block.properties, heading = Ordinary "section" }

        Just "subheading" ->
            { block | properties = Dict.insert "level" "4" block.properties, heading = Ordinary "section" }

        Just "item" ->
            { block | body = String.replace "- " "" block.firstLine :: block.body }

        Just "numbered" ->
            { block | body = String.replace ". " "" block.firstLine :: block.body }

        _ ->
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
                    if block.body == [ "" ] then
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
                , position = state.position
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
