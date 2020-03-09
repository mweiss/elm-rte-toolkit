module Rte.NodePath exposing
    ( commonAncestor
    , decrement
    , domToEditor
    , editorToDom
    , increment
    , parent
    , toString
    )

{-|

    This module contains functions related to transforming node paths.

    # DOM <-> Editor translation
    @docs domToEditor, editorToDom

-}

import Array exposing (Array)
import List.Extra
import Rte.Model
    exposing
        ( ChildNodes(..)
        , EditorBlockNode
        , EditorInlineLeaf(..)
        , ElementParameters
        , HtmlNode(..)
        , InlineLeafTree(..)
        , Mark
        , NodePath
        , Selection
        , Spec
        )
import Rte.Spec
    exposing
        ( childNodesPlaceholder
        , findMarkDefinitionFromSpecWithDefault
        , findNodeDefinitionFromSpecWithDefault
        )


domToEditorInlineLeafTree : Spec -> InlineLeafTree -> NodePath -> Maybe NodePath
domToEditorInlineLeafTree spec tree path =
    case tree of
        LeafNode i ->
            Just [ i ]

        MarkNode n ->
            let
                markDefinition =
                    findMarkDefinitionFromSpecWithDefault n.mark.name spec

                structure =
                    markDefinition.toHtmlNode n.mark childNodesPlaceholder
            in
            case removePathUpToChildContents structure path of
                Nothing ->
                    Nothing

                Just rest ->
                    case List.head rest of
                        Nothing ->
                            Just []

                        Just i ->
                            case Array.get i n.children of
                                Nothing ->
                                    Nothing

                                Just l ->
                                    domToEditorInlineLeafTree spec l (List.drop 1 rest)


{-| Translates a DOM node path to an editor node path. Returns Nothing if the
path is invalid.
-}
domToEditor : Spec -> EditorBlockNode -> NodePath -> Maybe NodePath
domToEditor spec node path =
    if List.isEmpty path then
        Just []

    else
        let
            nodeDefinition =
                findNodeDefinitionFromSpecWithDefault node.parameters.name spec

            structure =
                nodeDefinition.toHtmlNode node.parameters childNodesPlaceholder
        in
        case removePathUpToChildContents structure path of
            Nothing ->
                Nothing

            Just rest ->
                case List.head rest of
                    Nothing ->
                        Just []

                    Just i ->
                        case node.childNodes of
                            BlockArray l ->
                                case Array.get i l of
                                    Nothing ->
                                        Nothing

                                    Just childNode ->
                                        case domToEditor spec childNode (List.drop 1 rest) of
                                            Nothing ->
                                                Nothing

                                            Just p ->
                                                Just (i :: p)

                            InlineLeafArray l ->
                                case Array.get i l.tree of
                                    Nothing ->
                                        Nothing

                                    Just tree ->
                                        --  TODO:  we assume the content of the leaf node is valid, but maybe we should validate its content?
                                        domToEditorInlineLeafTree spec tree (List.drop 1 rest)

                            Leaf ->
                                -- If we still have path left, it means the path is invalid, so we return Nothing
                                Nothing


{-| Translates an editor node path to a DOM node path. Returns Nothing if the
path is invalid.
-}
editorToDom : Spec -> EditorBlockNode -> NodePath -> Maybe NodePath
editorToDom spec node path =
    case path of
        [] ->
            Just []

        x :: xs ->
            case pathToChildContentsFromElementParameters spec node.parameters of
                Nothing ->
                    Nothing

                Just childPath ->
                    case node.childNodes of
                        BlockArray l ->
                            case Array.get x l of
                                Nothing ->
                                    Nothing

                                Just childNode ->
                                    case editorToDom spec childNode xs of
                                        Nothing ->
                                            Nothing

                                        Just p ->
                                            Just (childPath ++ (x :: p))

                        InlineLeafArray l ->
                            case Array.get x l.reverseLookup of
                                Nothing ->
                                    Nothing

                                Just inlineTreePath ->
                                    case pathToChildContentsFromInlineTreePath spec l.array l.tree inlineTreePath of
                                        Nothing ->
                                            Nothing

                                        Just childInlineTreePath ->
                                            Just (childPath ++ childInlineTreePath)

                        Leaf ->
                            Nothing


increment : NodePath -> NodePath
increment np =
    case List.Extra.last np of
        Nothing ->
            []

        Just i ->
            List.take (List.length np - 1) np ++ [ i + 1 ]


decrement : NodePath -> NodePath
decrement np =
    case List.Extra.last np of
        Nothing ->
            []

        Just i ->
            List.take (List.length np - 1) np ++ [ i - 1 ]



{- Helper method to traverse the give node with the node path and return
   the node path that remains after finding the child nodes placeholder.  If no
   placeholder is found, then Nothing is returned.
-}


removePathUpToChildContents : HtmlNode -> NodePath -> Maybe NodePath
removePathUpToChildContents node path =
    case node of
        ElementNode _ _ children ->
            if children == childNodesPlaceholder then
                Just path

            else
                case path of
                    [] ->
                        Just path

                    x :: xs ->
                        case Array.get x children of
                            Nothing ->
                                Nothing

                            Just child ->
                                removePathUpToChildContents child xs

        TextNode _ ->
            Nothing



{- Helper method to return a node path to the which should contain the child contents. -}


pathToChildContents : HtmlNode -> Maybe NodePath
pathToChildContents node =
    case node of
        ElementNode _ _ children ->
            if children == childNodesPlaceholder then
                Just []

            else
                Array.foldl
                    (\( i, childNode ) maybePath ->
                        case maybePath of
                            Nothing ->
                                case pathToChildContents childNode of
                                    Nothing ->
                                        Nothing

                                    Just path ->
                                        Just (i :: path)

                            _ ->
                                maybePath
                    )
                    Nothing
                    (Array.indexedMap Tuple.pair children)

        TextNode _ ->
            Nothing



{- Helper method that returns the path to the child contents from a list of marks -}


pathToChildContentsFromMark : Spec -> Mark -> Maybe NodePath
pathToChildContentsFromMark spec mark =
    let
        markDefinition =
            findMarkDefinitionFromSpecWithDefault mark.name spec
    in
    let
        markStructure =
            markDefinition.toHtmlNode mark childNodesPlaceholder
    in
    pathToChildContents markStructure



{- Helper method to determine the path to the child contents from an element editor node -}


pathToChildContentsFromElementParameters : Spec -> ElementParameters -> Maybe NodePath
pathToChildContentsFromElementParameters spec parameters =
    let
        nodeDefinition =
            findNodeDefinitionFromSpecWithDefault parameters.name spec

        nodeStructure =
            nodeDefinition.toHtmlNode parameters childNodesPlaceholder
    in
    pathToChildContents nodeStructure


pathToChildContentsFromInlineTreePath : Spec -> Array EditorInlineLeaf -> Array InlineLeafTree -> NodePath -> Maybe NodePath
pathToChildContentsFromInlineTreePath spec array treeArray path =
    case path of
        [] ->
            Nothing

        x :: xs ->
            case Array.get x treeArray of
                Nothing ->
                    Nothing

                Just tree ->
                    case tree of
                        LeafNode i ->
                            case Array.get i array of
                                Nothing ->
                                    Nothing

                                Just _ ->
                                    Just [ x ]

                        MarkNode n ->
                            case pathToChildContentsFromMark spec n.mark of
                                Nothing ->
                                    Nothing

                                Just p ->
                                    case pathToChildContentsFromInlineTreePath spec array n.children xs of
                                        Nothing ->
                                            Nothing

                                        Just rest ->
                                            Just <| x :: p ++ rest


toString : NodePath -> String
toString nodePath =
    String.join ":" <| List.map String.fromInt nodePath


parent : NodePath -> NodePath
parent path =
    List.take (List.length path - 1) path


commonAncestor : NodePath -> NodePath -> NodePath
commonAncestor xPath yPath =
    case xPath of
        [] ->
            []

        x :: xs ->
            case yPath of
                [] ->
                    []

                y :: ys ->
                    if x == y then
                        x :: commonAncestor xs ys

                    else
                        []
