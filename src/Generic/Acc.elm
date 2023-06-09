module Generic.Acc exposing
    ( Accumulator
    , InitialAccumulatorData
    , init
    , initialAccumulator
    , initialData
    , transformAccumulate
    , transformST
    )

import Dict exposing (Dict)
import Either exposing (Either(..))
import Generic.ASTTools
import Generic.Forest exposing (Forest)
import Generic.Language exposing (Expr(..), Expression, ExpressionBlock, Heading(..))
import Generic.MathMacro
import Generic.Settings
import Generic.TextMacro exposing (Macro)
import Generic.Vector as Vector exposing (Vector)
import List.Extra
import Maybe.Extra
import Tree exposing (Tree)
import Utility


indentationQuantum =
    2


initialData : InitialAccumulatorData
initialData =
    { mathMacros = ""
    , textMacros = ""
    , vectorSize = 4
    }


type alias Accumulator =
    { headingIndex : Vector
    , documentIndex : Vector
    , counter : Dict String Int
    , blockCounter : Int
    , itemVector : Vector -- Used for section numbering
    , numberedItemDict : Dict String { level : Int, index : Int }
    , numberedBlockNames : List String
    , inList : Bool
    , reference : Dict String { id : String, numRef : String }
    , terms : Dict String TermLoc
    , footnotes : Dict String TermLoc
    , footnoteNumbers : Dict String Int
    , mathMacroDict : Generic.MathMacro.MathMacroDict
    , textMacroDict : Dict String Macro
    , keyValueDict : Dict String String
    , qAndAList : List ( String, String )
    , qAndADict : Dict String String
    }


initialAccumulator : Accumulator
initialAccumulator =
    { headingIndex = Vector.init 4
    , documentIndex = Vector.init 4
    , counter = Dict.empty
    , blockCounter = 0
    , itemVector = Vector.init 4
    , numberedItemDict = Dict.empty
    , numberedBlockNames = Generic.Settings.numberedBlockNames
    , inList = False
    , reference = Dict.empty
    , terms = Dict.empty
    , footnotes = Dict.empty
    , footnoteNumbers = Dict.empty
    , mathMacroDict = Dict.empty
    , textMacroDict = Dict.empty
    , keyValueDict = Dict.empty
    , qAndAList = []
    , qAndADict = Dict.empty
    }


getCounter : String -> Dict String Int -> Int
getCounter name dict =
    Dict.get name dict |> Maybe.withDefault 0


getCounterAsString : String -> Dict String Int -> String
getCounterAsString name dict =
    Dict.get name dict |> Maybe.map String.fromInt |> Maybe.withDefault ""


incrementCounter : String -> Dict String Int -> Dict String Int
incrementCounter name dict =
    Dict.insert name (getCounter name dict + 1) dict


transformST : InitialAccumulatorData -> Forest ExpressionBlock -> Forest ExpressionBlock
transformST data ast =
    ast |> transformAccumulate data |> Tuple.second


{-| Note that function transformAccumulate operates on initialized accumulator.
-}
transformAccumulate : InitialAccumulatorData -> Forest ExpressionBlock -> ( Accumulator, Forest ExpressionBlock )
transformAccumulate data forest =
    List.foldl (\tree ( acc_, ast_ ) -> transformAccumulateTree tree acc_ |> mapper ast_) ( init data, [] ) forest
        |> (\( acc_, ast_ ) -> ( acc_, List.reverse ast_ ))


type alias InitialAccumulatorData =
    { mathMacros : String
    , textMacros : String
    , vectorSize : Int
    }


init : InitialAccumulatorData -> Accumulator
init data =
    { headingIndex = Vector.init data.vectorSize
    , documentIndex = Vector.init data.vectorSize
    , inList = False
    , counter = Dict.empty
    , blockCounter = 0
    , itemVector = Vector.init data.vectorSize
    , numberedItemDict = Dict.empty
    , numberedBlockNames = Generic.Settings.numberedBlockNames
    , reference = Dict.empty
    , terms = Dict.empty
    , footnotes = Dict.empty
    , footnoteNumbers = Dict.empty
    , mathMacroDict = Dict.empty
    , textMacroDict = Dict.empty
    , keyValueDict = Dict.empty
    , qAndAList = []
    , qAndADict = Dict.empty
    }
        |> updateWithMathMacros data.mathMacros
        |> updateWithTextMacros data.textMacros


mapper ast_ ( acc_, tree_ ) =
    ( acc_, tree_ :: ast_ )


transformAccumulateBlock : Accumulator -> ExpressionBlock -> ( Accumulator, ExpressionBlock )
transformAccumulateBlock =
    \acc_ block_ ->
        let
            newAcc =
                updateAccumulator block_ acc_
        in
        ( newAcc, transformBlock newAcc block_ )


transformAccumulateTree : Tree ExpressionBlock -> Accumulator -> ( Accumulator, Tree ExpressionBlock )
transformAccumulateTree tree acc =
    Tree.mapAccumulate transformAccumulateBlock acc tree


{-|

    Add labels to blocks, e.g. number sections and equations

-}
transformBlock : Accumulator -> ExpressionBlock -> ExpressionBlock
transformBlock acc block =
    case ( block.heading, block.args ) of
        ( Ordinary "section", level :: [] ) ->
            { block | properties = Dict.insert "label" (Vector.toString acc.headingIndex) block.properties }

        ( Ordinary "quiver", _ ) ->
            { block | properties = Dict.insert "figure" (getCounterAsString "figure" acc.counter) block.properties }

        ( Ordinary "chart", _ ) ->
            { block | properties = Dict.insert "figure" (getCounterAsString "figure" acc.counter) block.properties }

        ( Ordinary "image", _ ) ->
            { block | properties = Dict.insert "figure" (getCounterAsString "figure" acc.counter) block.properties }

        ( Ordinary "iframe", _ ) ->
            { block | properties = Dict.insert "figure" (getCounterAsString "figure" acc.counter) block.properties }

        ( Ordinary "section", level :: "-" :: [] ) ->
            { block | args = level :: "-" :: [] }

        ( Ordinary "document", _ ) ->
            { block | properties = Dict.insert "label" (Vector.toString acc.documentIndex) block.properties }

        ( Verbatim "equation", _ ) ->
            let
                prefix =
                    Vector.toString acc.headingIndex

                equationProp =
                    if prefix == "" then
                        getCounterAsString "equation" acc.counter

                    else
                        Vector.toString acc.headingIndex ++ "." ++ getCounterAsString "equation" acc.counter
            in
            { block | properties = Dict.insert "equation" equationProp block.properties }

        ( Verbatim "aligned", _ ) ->
            let
                prefix =
                    Vector.toString acc.headingIndex

                equationProp =
                    if prefix == "" then
                        getCounterAsString "equation" acc.counter

                    else
                        Vector.toString acc.headingIndex ++ "." ++ getCounterAsString "equation" acc.counter
            in
            { block | properties = Dict.insert "equation" equationProp block.properties }

        ( heading, _ ) ->
            -- TODO: not at all sure that the below is correct
            case Generic.Language.getNameFromHeading heading of
                Nothing ->
                    block

                Just name ->
                    --{ block | properties = Dict.insert "label" name block.properties }
                    -- Insert the numerical counter, e.g,, equation number, in the arg list of the block
                    if List.member name [ "section" ] then
                        let
                            prefix =
                                Vector.toString acc.headingIndex

                            equationProp =
                                if prefix == "" then
                                    getCounterAsString "equation" acc.counter

                                else
                                    Vector.toString acc.headingIndex ++ "." ++ getCounterAsString "equation" acc.counter
                        in
                        { block
                            | properties = Dict.insert "label" equationProp block.properties
                        }

                    else
                        -- Default insertion of "label" property (used for block numbering)
                        (if List.member name Generic.Settings.numberedBlockNames then
                            { block
                                | properties = Dict.insert "label" (vectorPrefix acc.headingIndex ++ String.fromInt acc.blockCounter) block.properties
                            }

                         else
                            block
                        )
                            |> expand acc.textMacroDict


vectorPrefix : Vector -> String
vectorPrefix headingIndex =
    let
        prefix =
            Vector.toString headingIndex
    in
    if prefix == "" then
        ""

    else
        Vector.toString headingIndex ++ "."


{-| Map name to name of counter
-}
reduceName : String -> String
reduceName str =
    if List.member str [ "equation", "aligned" ] then
        "equation"

    else if str == "code" then
        "listing"

    else if List.member str [ "quiver", "image", "iframe", "chart", "datatable", "svg", "tikz", "iframe" ] then
        "figure"

    else
        str


expand : Dict String Macro -> ExpressionBlock -> ExpressionBlock
expand dict block =
    { block | body = Either.map (List.map (Generic.TextMacro.expand dict)) block.body }


{-| The first component of the return value (Bool, Maybe Vector) is the
updated inList.
-}
listData : Accumulator -> Maybe String -> ( Bool, Maybe Vector )
listData accumulator name =
    case ( accumulator.inList, name ) of
        ( False, Just "numbered" ) ->
            ( True, Just (Vector.init 4 |> Vector.increment 0) )

        ( False, Just "item" ) ->
            ( True, Just (Vector.init 4 |> Vector.increment 0) )

        ( _, Nothing ) ->
            -- Don't change state if there are anonymous blocks
            -- TODO: think about this, consistent with markdown semantics but not LaTeX
            -- TODO: however it does fix a numbering bug (see MicroLaTeX Visual OTNetworkTest)
            -- ( accumulator.inList, Nothing )
            ( False, Nothing )

        ( False, _ ) ->
            ( False, Nothing )

        ( True, Just "numbered" ) ->
            ( True, Nothing )

        ( True, Just "item" ) ->
            ( False, Nothing )

        ( True, _ ) ->
            ( False, Nothing )


type alias ReferenceDatum =
    { id : String
    , tag : String
    , numRef : String
    }


makeReferenceDatum : String -> String -> String -> ReferenceDatum
makeReferenceDatum id tag numRef =
    { id = id, tag = tag, numRef = numRef }


{-| Update the references dictionary: add a key-value pair where the
key is defined as in the examples \\label{foo} or [label foo],
and where value is a record with an id and a "numerical" reference,
e.g, "2" or "2.3"
-}
updateReference : ReferenceDatum -> Accumulator -> Accumulator
updateReference referenceDatum acc =
    if referenceDatum.tag /= "" then
        { acc
            | reference =
                Dict.insert referenceDatum.tag
                    { id = referenceDatum.id, numRef = referenceDatum.numRef }
                    acc.reference
        }

    else
        acc


updateReferenceWithBlock : ExpressionBlock -> Accumulator -> Accumulator
updateReferenceWithBlock block acc =
    case getReferenceDatum block of
        Just referenceDatum ->
            updateReference referenceDatum acc

        Nothing ->
            acc


getNameContentId : ExpressionBlock -> Maybe { name : String, content : Either String (List Expression), id : String }
getNameContentId block =
    let
        name =
            Dict.get "name" block.properties

        content : Maybe (Either String (List Expression))
        content =
            Just block.body

        id =
            Dict.get "id" block.properties
    in
    case ( name, content, id ) of
        ( Just name_, Just content_, Just id_ ) ->
            Just { name = name_, content = content_, id = id_ }

        _ ->
            Nothing


getNameContentIdTag : ExpressionBlock -> Maybe { name : String, content : Either String (List Expression), id : String, tag : String }
getNameContentIdTag block =
    let
        name =
            Dict.get "name" block.properties

        content : Maybe (Either String (List Expression))
        content =
            Just block.body

        id =
            Dict.get "id" block.properties

        tag =
            Dict.get "tag" block.properties
    in
    case tag of
        Nothing ->
            Nothing

        Just tag_ ->
            case ( name, content, id ) of
                ( Just name_, Just content_, Just id_ ) ->
                    Just { name = name_, content = content_, id = id_, tag = tag_ }

                _ ->
                    Nothing


getReferenceDatum : ExpressionBlock -> Maybe ReferenceDatum
getReferenceDatum block =
    let
        id =
            Dict.get "id" block.properties

        tag =
            Dict.get "tag" block.properties

        numRef =
            Dict.get "label" block.properties
    in
    case ( id, tag, numRef ) of
        ( Just id_, Just tag_, Just numRef_ ) ->
            Just { id = id_, tag = tag_, numRef = numRef_ }

        _ ->
            Nothing


updateAccumulator : ExpressionBlock -> Accumulator -> Accumulator
updateAccumulator ({ heading, indent, args, body, meta } as block) accumulator =
    -- Update the accumulator for expression blocks with selected name
    case heading of
        -- provide numbering for sections
        -- reference : Dict String { id : String, numRef : String }
        Ordinary "q" ->
            { accumulator
                | qAndAList = ( block.meta.id, "??" ) :: accumulator.qAndAList
                , blockCounter = accumulator.blockCounter + 1
            }
                |> updateReferenceWithBlock block

        Ordinary "a" ->
            case List.Extra.uncons accumulator.qAndAList of
                Just ( ( q, _ ), rest ) ->
                    { accumulator
                        | qAndAList = ( q, block.meta.id ) :: rest
                        , qAndADict = Dict.fromList (( q, block.meta.id ) :: rest)
                    }
                        |> updateReferenceWithBlock block

                _ ->
                    accumulator

        Ordinary "set-key" ->
            case args of
                key :: value :: rest ->
                    { accumulator | keyValueDict = Dict.insert key value accumulator.keyValueDict }

                _ ->
                    accumulator

        Ordinary "section" ->
            let
                level =
                    List.head args |> Maybe.withDefault "1"
            in
            case getNameContentId block of
                Just { name, content, id } ->
                    updateWithOrdinarySectionBlock accumulator (Just name) content level id

                Nothing ->
                    accumulator

        Ordinary "document" ->
            let
                level =
                    List.head args |> Maybe.withDefault "1"
            in
            case getNameContentId block of
                Just { name, content, id } ->
                    updateWithOrdinaryDocumentBlock accumulator (Just name) content level id

                _ ->
                    accumulator

        Ordinary "setcounter" ->
            let
                n =
                    List.head args |> Maybe.andThen String.toInt |> Maybe.withDefault 1
            in
            { accumulator | headingIndex = { content = [ n, 0, 0, 0 ], size = 4 } }

        Ordinary "bibitem" ->
            updateBibItemBlock accumulator args block.meta.id

        Ordinary name_ ->
            -- TODO: tighten up
            case getNameContentIdTag block of
                Just { name, content, id, tag } ->
                    accumulator |> updateWithOrdinaryBlock (Just name_) content tag id indent

                _ ->
                    accumulator

        -- provide for numbering of equations
        Verbatim "mathmacros" ->
            case Generic.Language.getVerbatimContent block of
                Nothing ->
                    accumulator

                Just str ->
                    updateWithMathMacros str accumulator

        Verbatim "textmacros" ->
            case Generic.Language.getVerbatimContent block of
                Nothing ->
                    accumulator

                Just str ->
                    updateWithTextMacros str accumulator

        Verbatim _ ->
            case getNameContentIdTag block of
                Nothing ->
                    accumulator

                Just { name, content, id, tag } ->
                    case content of
                        Left str ->
                            updateWithVerbatimBlock Nothing args str id accumulator

                        Right _ ->
                            accumulator

        Paragraph ->
            case getNameContentIdTag block of
                Nothing ->
                    accumulator

                Just { name, content, id, tag } ->
                    accumulator |> updateWithParagraph Nothing content id


normalzeLines : List String -> List String
normalzeLines lines =
    List.map (\line -> String.trim line) lines |> List.filter (\line -> line /= "")


updateWithOrdinarySectionBlock : Accumulator -> Maybe String -> Either String (List Expression) -> String -> String -> Accumulator
updateWithOrdinarySectionBlock accumulator name content level id =
    let
        ( inList, _ ) =
            listData accumulator name

        titleWords =
            case content of
                Left str ->
                    [ Utility.compressWhitespace str ]

                Right expr ->
                    List.map Generic.ASTTools.getText expr |> Maybe.Extra.values |> List.map Utility.compressWhitespace

        sectionTag =
            -- TODO: the below is a bad solution
            titleWords |> List.map (String.toLower >> Utility.compressWhitespace >> Utility.removeNonAlphaNum >> String.replace " " "-") |> String.join ""

        headingIndex =
            Vector.increment (String.toInt level |> Maybe.withDefault 2) accumulator.headingIndex

        blockCounter =
            0

        referenceDatum =
            makeReferenceDatum id sectionTag (Vector.toString headingIndex)
    in
    -- TODO: take care of numberedItemIndex = 0 here and elsewhere
    { accumulator
        | inList = inList
        , headingIndex = headingIndex
        , blockCounter = blockCounter
        , counter = Dict.insert "equation" 0 accumulator.counter
    }
        |> updateReference referenceDatum


updateWithOrdinaryDocumentBlock : Accumulator -> Maybe String -> Either String (List Expression) -> String -> String -> Accumulator
updateWithOrdinaryDocumentBlock accumulator name content level id =
    let
        ( inList, _ ) =
            listData accumulator name

        title =
            case content of
                Left str ->
                    str

                Right expr ->
                    List.map Generic.ASTTools.getText expr |> Maybe.Extra.values |> String.join " "

        sectionTag =
            title |> String.toLower |> String.replace " " "-"

        documentIndex =
            Vector.increment (String.toInt level |> Maybe.withDefault 0) accumulator.documentIndex

        referenceDatum =
            makeReferenceDatum id sectionTag (Vector.toString documentIndex)
    in
    -- TODO: take care of numberedItemIndex = 0 here and elsewhere
    { accumulator | inList = inList, documentIndex = documentIndex } |> updateReference referenceDatum


updateBibItemBlock accumulator args id =
    case List.head args of
        Nothing ->
            accumulator

        Just label ->
            { accumulator | reference = Dict.insert label { id = id, numRef = "_irrelevant_" } accumulator.reference }


updateWithOrdinaryBlock : Maybe String -> Either b (List Expression) -> String -> String -> Int -> Accumulator -> Accumulator
updateWithOrdinaryBlock name content tag id indent accumulator =
    let
        ( inList, initialNumberedVector ) =
            listData accumulator name
    in
    case name of
        Just "setcounter" ->
            case content of
                Left _ ->
                    accumulator

                Right exprs ->
                    let
                        ctr =
                            case exprs of
                                [ Text val _ ] ->
                                    String.toInt val |> Maybe.withDefault 1

                                _ ->
                                    1

                        headingIndex =
                            Vector.init accumulator.headingIndex.size |> Vector.set 0 (ctr - 1)
                    in
                    { accumulator | headingIndex = headingIndex }

        Just "numbered" ->
            let
                level =
                    indent // indentationQuantum

                itemVector =
                    case initialNumberedVector of
                        Just v ->
                            v

                        Nothing ->
                            Vector.increment level accumulator.itemVector

                index =
                    Vector.get level itemVector

                numberedItemDict =
                    Dict.insert id { level = level, index = index } accumulator.numberedItemDict

                referenceDatum =
                    makeReferenceDatum id tag (String.fromInt (Vector.get level itemVector))
            in
            { accumulator | inList = inList, itemVector = itemVector, numberedItemDict = numberedItemDict }
                |> updateReference referenceDatum

        Just "item" ->
            let
                level =
                    indent // indentationQuantum

                itemVector =
                    case initialNumberedVector of
                        Just v ->
                            v

                        Nothing ->
                            Vector.increment level accumulator.itemVector

                numberedItemDict =
                    Dict.insert id { level = level, index = Vector.get level itemVector } accumulator.numberedItemDict

                referenceDatum =
                    makeReferenceDatum id tag (String.fromInt (Vector.get level itemVector))
            in
            { accumulator | inList = inList, itemVector = itemVector, numberedItemDict = numberedItemDict }
                |> updateReference referenceDatum

        Just name_ ->
            if List.member name_ [ "title", "contents", "banner", "a" ] then
                accumulator

            else if List.member name_ Generic.Settings.numberedBlockNames then
                --- TODO: fix thereom labels
                let
                    level =
                        indent // indentationQuantum

                    itemVector =
                        Vector.increment level accumulator.itemVector

                    numberedItemDict =
                        Dict.insert id { level = level, index = Vector.get level itemVector } accumulator.numberedItemDict

                    referenceDatum =
                        makeReferenceDatum id tag (String.fromInt (Vector.get level itemVector))
                in
                { accumulator | blockCounter = accumulator.blockCounter + 1, itemVector = itemVector, numberedItemDict = numberedItemDict }
                    |> updateReference referenceDatum

            else
                accumulator

        _ ->
            accumulator


updateWithTextMacros : String -> Accumulator -> Accumulator
updateWithTextMacros content accumulator =
    { accumulator | textMacroDict = Generic.TextMacro.buildDictionary (String.lines content |> normalzeLines) }


updateWithMathMacros : String -> Accumulator -> Accumulator
updateWithMathMacros content accumulator =
    let
        definitions =
            content
                |> String.replace "\\begin{mathmacros}" ""
                |> String.replace "\\end{mathmacros}" ""
                |> String.replace "end" ""
                |> String.trim

        mathMacroDict =
            Generic.MathMacro.makeMacroDict (String.trim definitions)
    in
    { accumulator | mathMacroDict = mathMacroDict }


updateWithVerbatimBlock : Maybe String -> List String -> String -> String -> Accumulator -> Accumulator
updateWithVerbatimBlock name_ args tag_ id accumulator =
    let
        ( inList, _ ) =
            listData accumulator name_

        name =
            Maybe.withDefault "---" name_

        dict =
            Utility.keyValueDict args

        tag =
            Dict.get "label" dict |> Maybe.withDefault tag_

        isSimple =
            List.member name [ "quiver", "image" ]

        -- Increment the appropriate counter, e.g., "equation" and "aligned"
        -- reduceName maps these both to "equation"
        newCounter =
            if List.member name accumulator.numberedBlockNames then
                incrementCounter (reduceName name) accumulator.counter

            else
                accumulator.counter

        referenceDatum =
            makeReferenceDatum id tag (verbatimBlockReference isSimple accumulator.headingIndex name newCounter)
    in
    { accumulator | inList = inList, counter = newCounter }
        |> updateReference referenceDatum


verbatimBlockReference : Bool -> Vector -> String -> Dict String Int -> String
verbatimBlockReference isSimple headingIndex name newCounter =
    let
        a =
            Vector.toString headingIndex
    in
    if a == "" || isSimple then
        getCounter (reduceName name) newCounter |> String.fromInt

    else
        a ++ "." ++ (getCounter (reduceName name) newCounter |> String.fromInt)


updateWithParagraph name content id accumulator =
    let
        ( inList, _ ) =
            listData accumulator name

        ( footnotes, footnoteNumbers ) =
            addFootnotesFromContent id content ( accumulator.footnotes, accumulator.footnoteNumbers )
    in
    { accumulator
        | inList = inList
        , terms = addTermsFromContent id content accumulator.terms
        , footnotes = footnotes
        , footnoteNumbers = footnoteNumbers
    }



--|> updateReference tag id tag


type alias TermLoc =
    { begin : Int, end : Int, id : String }


type alias TermData =
    { term : String, loc : TermLoc }


getTerms : String -> Either String (List Expression) -> List TermData
getTerms id content_ =
    case content_ of
        Right expressionList ->
            Generic.ASTTools.filterExpressionsOnName_ "term" expressionList
                |> List.map (extract id)
                |> Maybe.Extra.values

        Left _ ->
            []



-- TERMS: [Expression "term" [Text "group" { begin = 19, end = 23, index = 4 }] { begin = 13, end = 13, index = 1 }]


extract : String -> Expression -> Maybe TermData
extract id expr =
    case expr of
        Fun "term" [ Text name { begin, end } ] _ ->
            Just { term = name, loc = { begin = begin, end = end, id = id } }

        Fun "term_" [ Text name { begin, end } ] _ ->
            Just { term = name, loc = { begin = begin, end = end, id = id } }

        _ ->
            Nothing


addTerm : TermData -> Dict String TermLoc -> Dict String TermLoc
addTerm termData dict =
    Dict.insert termData.term termData.loc dict


addTerms : List TermData -> Dict String TermLoc -> Dict String TermLoc
addTerms termDataList dict =
    List.foldl addTerm dict termDataList


addTermsFromContent : String -> Either String (List Expression) -> Dict String TermLoc -> Dict String TermLoc
addTermsFromContent id content dict =
    addTerms (getTerms id content) dict



-- FOOTNOTES


getFootnotes : String -> Either String (List Expression) -> List TermData
getFootnotes id content_ =
    case content_ of
        Right expressionList ->
            Generic.ASTTools.filterExpressionsOnName_ "footnote" expressionList
                |> List.map (extractFootnote id)
                |> Maybe.Extra.values

        Left _ ->
            []


extractFootnote : String -> Expression -> Maybe TermData
extractFootnote id_ expr =
    case expr of
        Fun "footnote" [ Text content { begin, end, index, id } ] _ ->
            Just { term = content, loc = { begin = begin, end = end, id = id } }

        _ ->
            Nothing



-- EXTRACT ??


addFootnote : TermData -> Dict String TermLoc -> Dict String TermLoc
addFootnote footnoteData dict =
    Dict.insert footnoteData.term footnoteData.loc dict


addFootnoteLabel : TermData -> Dict String Int -> Dict String Int
addFootnoteLabel footnoteData dict =
    Dict.insert footnoteData.loc.id (Dict.size dict + 1) dict


addFootnotes : List TermData -> ( Dict String TermLoc, Dict String Int ) -> ( Dict String TermLoc, Dict String Int )
addFootnotes termDataList ( dict1, dict2 ) =
    List.foldl (\data ( d1, d2 ) -> ( addFootnote data d1, addFootnoteLabel data d2 )) ( dict1, dict2 ) termDataList


addFootnotesFromContent : String -> Either String (List Expression) -> ( Dict String TermLoc, Dict String Int ) -> ( Dict String TermLoc, Dict String Int )
addFootnotesFromContent id content ( dict1, dict2 ) =
    addFootnotes (getFootnotes id content) ( dict1, dict2 )
