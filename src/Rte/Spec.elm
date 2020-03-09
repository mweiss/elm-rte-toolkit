module Rte.Spec exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html.Parser exposing (Node(..))
import List.Extra
import Result exposing (Result)
import Rte.Marks exposing (ToggleAction(..), toggle)
import Rte.Model
    exposing
        ( ChildNodes(..)
        , ContentType(..)
        , EditorAttribute(..)
        , EditorBlockNode
        , EditorFragment(..)
        , EditorInlineLeaf(..)
        , EditorNode(..)
        , EditorState
        , ElementParameters
        , HtmlNode(..)
        , Mark
        , MarkDefinition
        , MarkOrder
        , NodeDefinition
        , Spec
        , blockLeafContentType
        , blockNodeContentType
        , inlineLeafArray
        , markDefinition
        , nodeDefinition
        , zeroWidthSpace
        )
import Set exposing (Set)


emptySpec : Spec
emptySpec =
    { nodes = [], marks = [] }


childNodesPlaceholder =
    Array.fromList
        [ ElementNode "__child_node_marker__" [] Array.empty ]


defaultElementToHtml : String -> ElementParameters -> Array HtmlNode -> HtmlNode
defaultElementToHtml tagName elementParameters children =
    ElementNode tagName
        (List.filterMap
            (\attr ->
                case attr of
                    StringAttribute k v ->
                        Just ( k, v )

                    _ ->
                        Nothing
            )
            elementParameters.attributes
        )
        children


defaultHtmlToElement : String -> String -> HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )
defaultHtmlToElement htmlTag elementName node =
    case node of
        ElementNode name _ children ->
            if name == htmlTag then
                Just ( { name = elementName, attributes = [], annotations = Set.empty }, children )

            else
                Nothing

        _ ->
            Nothing


defaultHtmlToMark : String -> String -> HtmlNode -> Maybe ( Mark, Array HtmlNode )
defaultHtmlToMark htmlTag markName node =
    case node of
        ElementNode name _ children ->
            if name == htmlTag then
                Just ( { name = markName, attributes = [] }, children )

            else
                Nothing

        _ ->
            Nothing


defaultMarkToHtml : Mark -> Array HtmlNode -> HtmlNode
defaultMarkToHtml mark children =
    ElementNode mark.name
        (List.filterMap
            (\attr ->
                case attr of
                    StringAttribute k v ->
                        Just ( k, v )

                    _ ->
                        Nothing
            )
            mark.attributes
        )
        children


findNodeDefinitionFromSpec : String -> Spec -> Maybe NodeDefinition
findNodeDefinitionFromSpec name spec =
    List.Extra.find (\n -> n.name == name) spec.nodes


findNodeDefinitionFromSpecWithDefault : String -> Spec -> NodeDefinition
findNodeDefinitionFromSpecWithDefault name spec =
    Maybe.withDefault
        (nodeDefinition
            name
            "block"
            (blockNodeContentType [])
            (defaultElementToHtml name)
            (defaultHtmlToElement name name)
        )
        (findNodeDefinitionFromSpec name spec)


findMarkDefinitionFromSpec : String -> Spec -> Maybe MarkDefinition
findMarkDefinitionFromSpec name spec =
    List.Extra.find (\n -> n.name == name) spec.marks


findMarkDefinitionFromSpecWithDefault : String -> Spec -> MarkDefinition
findMarkDefinitionFromSpecWithDefault name spec =
    Maybe.withDefault
        (markDefinition
            name
            defaultMarkToHtml
            (defaultHtmlToMark name name)
        )
        (findMarkDefinitionFromSpec name spec)


validate : Spec -> EditorState -> Result String EditorState
validate spec editorState =
    let
        root =
            editorState.root
    in
    case validateEditorBlockNode spec Nothing root of
        [] ->
            Ok editorState

        result ->
            Err <| String.join ", " result


toStringContentType : ContentType -> String
toStringContentType contentType =
    case contentType of
        TextBlockNodeType _ ->
            "TextBlockNodeType"

        InlineLeafNodeType ->
            "InlineLeafNodeType"

        BlockNodeType _ ->
            "BlockNodeType"

        BlockLeafNodeType ->
            "BlockLeafNodeType"


validateInlineLeaf : Spec -> Maybe (Set String) -> EditorInlineLeaf -> List String
validateInlineLeaf spec allowedGroups leaf =
    case leaf of
        TextLeaf _ ->
            []

        InlineLeaf il ->
            case findNodeDefinitionFromSpec il.parameters.name spec of
                Nothing ->
                    [ "Cannot find node with definition '" ++ il.parameters.name ++ "'" ]

                Just definition ->
                    validateAllowedGroups allowedGroups definition.group


validateAllowedGroups : Maybe (Set String) -> String -> List String
validateAllowedGroups allowedGroups group =
    case allowedGroups of
        Nothing ->
            []

        Just groups ->
            if Set.member group groups then
                []

            else
                [ "Group "
                    ++ group
                    ++ " is not in allowed groups {"
                    ++ String.join ", " (Set.toList groups)
                    ++ "}"
                ]


validateEditorBlockNode : Spec -> Maybe (Set String) -> EditorBlockNode -> List String
validateEditorBlockNode spec allowedGroups node =
    case findNodeDefinitionFromSpec node.parameters.name spec of
        Nothing ->
            [ "Cannot find node with definition '" ++ node.parameters.name ++ "'" ]

        Just definition ->
            let
                allowedGroupsErrors =
                    validateAllowedGroups allowedGroups definition.group
            in
            if not <| List.isEmpty allowedGroupsErrors then
                allowedGroupsErrors

            else
                case node.childNodes of
                    BlockArray ba ->
                        case definition.contentType of
                            BlockNodeType groups ->
                                List.concatMap
                                    (validateEditorBlockNode spec groups)
                                    (Array.toList ba)

                            _ ->
                                [ "I was expecting textblock content type, but instead I got "
                                    ++ toStringContentType definition.contentType
                                ]

                    InlineLeafArray la ->
                        case definition.contentType of
                            TextBlockNodeType groups ->
                                List.concatMap (validateInlineLeaf spec groups) (Array.toList la.array)

                            _ ->
                                [ "I was expecting textblock content type, but instead I got " ++ toStringContentType definition.contentType ]

                    Leaf ->
                        if definition.contentType == blockLeafContentType then
                            []

                        else
                            [ "I was expecting leaf blockleaf content type, but instead I got "
                                ++ toStringContentType definition.contentType
                            ]


resultFilterMap : (a -> Result c b) -> Array a -> Array b
resultFilterMap f xs =
    let
        maybePush : (a -> Result c b) -> a -> Array b -> Array b
        maybePush f_ mx xs_ =
            case f_ mx of
                Ok x ->
                    Array.push x xs_

                Err _ ->
                    xs_
    in
    Array.foldl (maybePush f) Array.empty xs


htmlToElementArray : Spec -> String -> Result String (Array EditorFragment)
htmlToElementArray spec html =
    case stringToHtmlNodeArray html of
        Err s ->
            Err s

        Ok htmlNodeArray ->
            let
                newArray =
                    resultFilterMap (htmlNodeToEditorFragment spec []) htmlNodeArray
            in
            if Array.length newArray /= Array.length htmlNodeArray then
                Err "Could not create a valid editor node array from html node array"

            else
                Ok <| reduceEditorFragmentArray newArray


htmlNodeToEditorFragment : Spec -> List Mark -> HtmlNode -> Result String EditorFragment
htmlNodeToEditorFragment spec marks node =
    case node of
        TextNode s ->
            Ok <|
                InlineLeafFragment <|
                    Array.fromList
                        [ TextLeaf
                            { text =
                                String.replace zeroWidthSpace "" s
                            , marks = marks
                            , annotations = Set.empty
                            }
                        ]

        _ ->
            let
                nodeDefinitions =
                    spec.nodes

                maybeElementAndChildren =
                    List.foldl
                        (\definition result ->
                            case result of
                                Nothing ->
                                    case definition.fromHtmlNode node of
                                        Nothing ->
                                            Nothing

                                        Just v ->
                                            Just ( definition, v )

                                Just _ ->
                                    result
                        )
                        Nothing
                        nodeDefinitions
            in
            case maybeElementAndChildren of
                Just ( definition, ( element, children ) ) ->
                    if definition.contentType == InlineLeafNodeType then
                        Ok <| InlineLeafFragment <| Array.fromList [ InlineLeaf { marks = marks, parameters = element } ]

                    else
                        let
                            childArr =
                                Array.map (htmlNodeToEditorFragment spec []) children
                        in
                        case arrayToChildNodes definition.contentType childArr of
                            Err s ->
                                Err s

                            Ok childNodes ->
                                Ok <| BlockNodeFragment <| Array.fromList [ { parameters = element, childNodes = childNodes } ]

                Nothing ->
                    case htmlNodeToMark spec node of
                        Nothing ->
                            Err "No mark or node matches the spec"

                        Just ( mark, children ) ->
                            let
                                newMarks =
                                    toggle Add (markOrderFromSpec spec) mark marks

                                newChildren =
                                    Array.map (htmlNodeToEditorFragment spec newMarks) children
                            in
                            arrayToFragment newChildren


htmlNodeToMark : Spec -> HtmlNode -> Maybe ( Mark, Array HtmlNode )
htmlNodeToMark spec node =
    List.foldl
        (\definition result ->
            case result of
                Nothing ->
                    case definition.fromHtmlNode node of
                        Nothing ->
                            Nothing

                        Just m ->
                            Just m

                Just _ ->
                    result
        )
        Nothing
        spec.marks


reduceEditorFragmentArray : Array EditorFragment -> Array EditorFragment
reduceEditorFragmentArray fragmentArray =
    Array.foldl
        (\fragment arr ->
            case Array.get (Array.length arr - 1) arr of
                Nothing ->
                    Array.push fragment arr

                Just prevFragment ->
                    case prevFragment of
                        InlineLeafFragment pilf ->
                            case fragment of
                                InlineLeafFragment ilf ->
                                    Array.set (Array.length arr - 1) (InlineLeafFragment (Array.append pilf ilf)) arr

                                BlockNodeFragment _ ->
                                    Array.push fragment arr

                        BlockNodeFragment pbnf ->
                            case fragment of
                                InlineLeafFragment _ ->
                                    Array.push fragment arr

                                BlockNodeFragment bnf ->
                                    Array.set (Array.length arr - 1) (BlockNodeFragment (Array.append pbnf bnf)) arr
        )
        Array.empty
        fragmentArray


arrayToChildNodes : ContentType -> Array (Result String EditorFragment) -> Result String ChildNodes
arrayToChildNodes contentType results =
    if Array.isEmpty results then
        case contentType of
            BlockLeafNodeType ->
                Ok Leaf

            _ ->
                Err "Invalid node type for empty fragment result array"

    else
        case arrayToFragment results of
            Err e ->
                Err e

            Ok fragment ->
                case fragment of
                    InlineLeafFragment ilf ->
                        case contentType of
                            TextBlockNodeType _ ->
                                Ok <| inlineLeafArray ilf

                            _ ->
                                Err "I received an inline leaf fragment, but the node I parsed doesn't accept this child type"

                    BlockNodeFragment bnf ->
                        case contentType of
                            BlockNodeType _ ->
                                Ok <| BlockArray bnf

                            _ ->
                                Err "I received a block node fragment, but the node I parsed doesn't accept this child type"


arrayToFragment : Array (Result String EditorFragment) -> Result String EditorFragment
arrayToFragment results =
    let
        aResult =
            Array.foldl
                (\fragmentResult arrayResult ->
                    case arrayResult of
                        Err e ->
                            Err e

                        Ok arr ->
                            case fragmentResult of
                                Err e ->
                                    Err e

                                Ok fragment ->
                                    Ok <| Array.push fragment arr
                )
                (Ok Array.empty)
                results
    in
    case aResult of
        Err e ->
            Err e

        Ok result ->
            let
                reducedArray =
                    reduceEditorFragmentArray result
            in
            case Array.get 0 reducedArray of
                Nothing ->
                    Err "Unable to parse an editor fragment from the results"

                Just fragment ->
                    if Array.length reducedArray /= 1 then
                        Err "I received both inline and block fragments, which is invalid."

                    else
                        Ok fragment


stringToHtmlNodeArray : String -> Result String (Array HtmlNode)
stringToHtmlNodeArray html =
    case Html.Parser.run html of
        Err _ ->
            Err "Could not parse html string"

        Ok nodeList ->
            Ok <| nodeListToHtmlNodeArray nodeList


nodeListToHtmlNodeArray : List Node -> Array HtmlNode
nodeListToHtmlNodeArray nodeList =
    Array.fromList <|
        List.concatMap
            (\n ->
                case n of
                    Element name attributes children ->
                        [ ElementNode name attributes <| nodeListToHtmlNodeArray children ]

                    Text s ->
                        [ TextNode s ]

                    Comment _ ->
                        []
            )
            nodeList


markOrderFromSpec : Spec -> MarkOrder
markOrderFromSpec spec =
    Dict.fromList (List.indexedMap (\i m -> ( m.name, i )) spec.marks)
