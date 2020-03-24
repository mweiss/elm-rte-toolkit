module RichTextEditor.Internal.Spec exposing (htmlToElementArray, markDefinitionWithDefault, nodeDefinitionWithDefault)

import Array exposing (Array)
import Html.Parser as Html exposing (Node(..))
import Result exposing (Result)
import RichTextEditor.Config.MarkDefinition as MarkDefinition exposing (MarkDefinition, defaultMarkDefinition)
import RichTextEditor.Config.NodeDefinition as NodeDefinition exposing (NodeDefinition, blockNode, defaultNodeDefinition)
import RichTextEditor.Config.Spec exposing (Spec, markDefinition, markDefinitions, nodeDefinition, nodeDefinitions)
import RichTextEditor.Internal.Constants exposing (zeroWidthSpace)
import RichTextEditor.Internal.Model.Definitions exposing (ContentType(..), nameFromElement, nameFromMark)
import RichTextEditor.Model.Element exposing (Element)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.InlineElement exposing (inlineElement)
import RichTextEditor.Model.Mark
    exposing
        ( Mark
        , MarkOrder(..)
        , ToggleAction(..)
        , mark
        , markOrderFromSpec
        , toggle
        )
import RichTextEditor.Model.Node as Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , blockChildren
        , childNodes
        , inlineChildren
        )
import RichTextEditor.Model.Text as Text
import RichTextEditor.Node exposing (Fragment(..))


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
                        [ Node.Text <|
                            (Text.empty
                                |> Text.withText (String.replace zeroWidthSpace "" s)
                                |> Text.withMarks marks
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
                                    case NodeDefinition.fromHtmlNode definition definition node of
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
                            NodeDefinition.contentType definition
                    in
                    if contentType == InlineLeafNodeType then
                        Ok <|
                            InlineLeafFragment <|
                                Array.fromList
                                    [ Node.InlineElement <|
                                        inlineElement element marks
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
                                Ok <| BlockNodeFragment <| Array.fromList [ Node.block element childNodes ]

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
                    case MarkDefinition.fromHtmlNode definition definition node of
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


arrayToChildNodes : ContentType -> Array (Result String Fragment) -> Result String Children
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
                                Ok <| inlineChildren ilf

                            _ ->
                                Err "I received an inline leaf fragment, but the node I parsed doesn't accept this child type"

                    BlockNodeFragment bnf ->
                        case contentType of
                            BlockNodeType _ ->
                                Ok <| blockChildren bnf

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
    case Html.run html of
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
                    Html.Element name attributes children ->
                        -- We filter meta tags because chrome adds it to the pasted text/html
                        if String.toLower name /= "meta" then
                            [ ElementNode name attributes <| nodeListToHtmlNodeArray children ]

                        else
                            []

                    Html.Text s ->
                        [ TextNode s ]

                    Html.Comment _ ->
                        []
            )
            nodeList


markDefinitionWithDefault : Mark -> Spec -> MarkDefinition
markDefinitionWithDefault mark spec =
    let
        name =
            nameFromMark mark
    in
    Maybe.withDefault (defaultMarkDefinition name) (markDefinition name spec)


nodeDefinitionWithDefault : Element -> Spec -> NodeDefinition
nodeDefinitionWithDefault ele spec =
    let
        name =
            nameFromElement ele
    in
    Maybe.withDefault (defaultNodeDefinition name "block" (blockNode [])) (nodeDefinition name spec)
