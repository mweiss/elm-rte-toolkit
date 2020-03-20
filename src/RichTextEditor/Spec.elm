module RichTextEditor.Spec exposing
    ( childNodesPlaceholder
    , defaultElementToHtml
    , defaultHtmlToElement
    , defaultHtmlToMark
    , defaultMarkToHtml
    , htmlToElementArray
    , markOrderFromSpec
    , validate
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Html.Parser exposing (Node(..))
import Result exposing (Result)
import RichTextEditor.Model.Attribute exposing (Attribute(..))
import RichTextEditor.Model.Constants exposing (zeroWidthSpace)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Internal.Spec exposing (ContentType(..))
import RichTextEditor.Model.Mark as Mark
    exposing
        ( Mark
        , MarkOrder(..)
        , ToggleAction(..)
        , mark
        , toggle
        )
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , ChildNodes(..)
        , ElementParameters
        , Fragment(..)
        , InlineLeaf(..)
        , attributesFromElementParameters
        , blockArray
        , blockNode
        , childNodes
        , definitionFromElementParameters
        , elementParameters
        , elementParametersFromBlockNode
        , elementParametersFromInlineLeafParameters
        , emptyTextLeafParameters
        , fromBlockArray
        , fromInlineArray
        , inlineLeafArray
        , inlineLeafParameters
        , textLeafParametersWithMarks
        , withText
        )
import RichTextEditor.Model.Spec
    exposing
        ( MarkDefinition
        , NodeDefinition
        , Spec
        , blockLeafContentType
        , contentTypeFromNodeDefinition
        , fromHtmlNodeFromMarkDefinition
        , fromHtmlNodeFromNodeDefinition
        , groupFromNodeDefinition
        , markDefinitions
        , nameFromMarkDefinition
        , nameFromNodeDefinition
        , nodeDefinitions
        )
import RichTextEditor.Model.State as State exposing (State)
import Set exposing (Set)


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
            (attributesFromElementParameters elementParameters)
        )
        children


defaultHtmlToElement : String -> NodeDefinition -> HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )
defaultHtmlToElement htmlTag def node =
    case node of
        ElementNode name _ children ->
            if name == htmlTag then
                Just ( elementParameters def [] Set.empty, children )

            else
                Nothing

        _ ->
            Nothing


defaultHtmlToMark : String -> MarkDefinition -> HtmlNode -> Maybe ( Mark, Array HtmlNode )
defaultHtmlToMark htmlTag def node =
    case node of
        ElementNode name _ children ->
            if name == htmlTag then
                Just ( mark def [], children )

            else
                Nothing

        _ ->
            Nothing


defaultMarkToHtml : Mark -> Array HtmlNode -> HtmlNode
defaultMarkToHtml mark children =
    ElementNode (Mark.name mark)
        (List.filterMap
            (\attr ->
                case attr of
                    StringAttribute k v ->
                        Just ( k, v )

                    _ ->
                        Nothing
            )
            (Mark.attributes mark)
        )
        children


validate : State -> Result String State
validate editorState =
    let
        root =
            State.root editorState
    in
    case validateEditorBlockNode (Just <| Set.singleton "root") root of
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


validateInlineLeaf : Maybe (Set String) -> InlineLeaf -> List String
validateInlineLeaf allowedGroups leaf =
    case leaf of
        TextLeaf _ ->
            []

        InlineLeaf il ->
            let
                definition =
                    definitionFromElementParameters (elementParametersFromInlineLeafParameters il)
            in
            validateAllowedGroups allowedGroups (groupFromNodeDefinition definition) (nameFromNodeDefinition definition)


validateAllowedGroups : Maybe (Set String) -> String -> String -> List String
validateAllowedGroups allowedGroups group name =
    case allowedGroups of
        Nothing ->
            []

        Just groups ->
            if Set.member group groups then
                []

            else if Set.member name groups then
                []

            else
                [ "Group "
                    ++ group
                    ++ " is not in allowed groups {"
                    ++ String.join ", " (Set.toList groups)
                    ++ "}"
                ]


validateEditorBlockNode : Maybe (Set String) -> BlockNode -> List String
validateEditorBlockNode allowedGroups node =
    let
        parameters =
            elementParametersFromBlockNode node

        definition =
            definitionFromElementParameters parameters
    in
    let
        allowedGroupsErrors =
            validateAllowedGroups allowedGroups (groupFromNodeDefinition definition) (nameFromNodeDefinition definition)
    in
    if not <| List.isEmpty allowedGroupsErrors then
        allowedGroupsErrors

    else
        let
            contentType =
                contentTypeFromNodeDefinition definition
        in
        case childNodes node of
            BlockChildren ba ->
                case contentType of
                    BlockNodeType groups ->
                        List.concatMap
                            (validateEditorBlockNode groups)
                            (Array.toList (fromBlockArray ba))

                    _ ->
                        [ "I was expecting textblock content type, but instead I got "
                            ++ toStringContentType contentType
                        ]

            InlineChildren la ->
                case contentType of
                    TextBlockNodeType groups ->
                        List.concatMap (validateInlineLeaf groups) (Array.toList (fromInlineArray la))

                    _ ->
                        [ "I was expecting textblock content type, but instead I got " ++ toStringContentType contentType ]

            Leaf ->
                if contentType == blockLeafContentType then
                    []

                else
                    [ "I was expecting leaf blockleaf content type, but instead I got "
                        ++ toStringContentType contentType
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


htmlToElementArray : Spec -> String -> Result String (Array Fragment)
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


htmlNodeToEditorFragment : Spec -> List Mark -> HtmlNode -> Result String Fragment
htmlNodeToEditorFragment spec marks node =
    case node of
        TextNode s ->
            Ok <|
                InlineLeafFragment <|
                    Array.fromList
                        [ TextLeaf <|
                            (emptyTextLeafParameters
                                |> withText (String.replace zeroWidthSpace "" s)
                                |> textLeafParametersWithMarks marks
                            )
                        ]

        _ ->
            let
                definitions =
                    nodeDefinitions spec

                maybeElementAndChildren =
                    List.foldl
                        (\definition result ->
                            case result of
                                Nothing ->
                                    case fromHtmlNodeFromNodeDefinition definition definition node of
                                        Nothing ->
                                            Nothing

                                        Just v ->
                                            Just ( definition, v )

                                Just _ ->
                                    result
                        )
                        Nothing
                        definitions
            in
            case maybeElementAndChildren of
                Just ( definition, ( element, children ) ) ->
                    let
                        contentType =
                            contentTypeFromNodeDefinition definition
                    in
                    if contentType == InlineLeafNodeType then
                        Ok <|
                            InlineLeafFragment <|
                                Array.fromList
                                    [ InlineLeaf <|
                                        inlineLeafParameters element marks
                                    ]

                    else
                        let
                            childArr =
                                Array.map (htmlNodeToEditorFragment spec []) children
                        in
                        case arrayToChildNodes contentType childArr of
                            Err s ->
                                Err s

                            Ok childNodes ->
                                Ok <| BlockNodeFragment <| Array.fromList [ blockNode element childNodes ]

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
                    case fromHtmlNodeFromMarkDefinition definition definition node of
                        Nothing ->
                            Nothing

                        Just m ->
                            Just m

                Just _ ->
                    result
        )
        Nothing
        (markDefinitions spec)


reduceEditorFragmentArray : Array Fragment -> Array Fragment
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


arrayToChildNodes : ContentType -> Array (Result String Fragment) -> Result String ChildNodes
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
                                Ok <| blockArray bnf

                            _ ->
                                Err "I received a block node fragment, but the node I parsed doesn't accept this child type"


arrayToFragment : Array (Result String Fragment) -> Result String Fragment
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
                        -- We filter meta tags because chrome adds it to the pasted text/html
                        if String.toLower name /= "meta" then
                            [ ElementNode name attributes <| nodeListToHtmlNodeArray children ]

                        else
                            []

                    Text s ->
                        [ TextNode s ]

                    Comment _ ->
                        []
            )
            nodeList


markOrderFromSpec : Spec -> MarkOrder
markOrderFromSpec spec =
    MarkOrder <| Dict.fromList (List.indexedMap (\i m -> ( nameFromMarkDefinition m, i )) (markDefinitions spec))
