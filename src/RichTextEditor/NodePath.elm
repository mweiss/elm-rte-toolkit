module RichTextEditor.NodePath exposing
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
import RichTextEditor.Model.Element as Element exposing (Element)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Mark as Mark exposing (Mark)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , ChildNodes(..)
        , InlineLeaf
        , InlineLeafTree(..)
        , Path
        , childNodes
        , elementFromBlockNode
        , fromBlockArray
        , fromInlineArray
        , reverseLookupFromInlineArray
        , treeFromInlineArray
        )
import RichTextEditor.Model.Spec
    exposing
        ( Spec
        , toHtmlNodeFromMarkDefinition
        , toHtmlNodeFromNodeDefinition
        )
import RichTextEditor.Spec
    exposing
        ( childNodesPlaceholder
        )


domToEditorInlineLeafTree : InlineLeafTree -> Path -> Maybe Path
domToEditorInlineLeafTree tree path =
    case tree of
        LeafNode i ->
            Just [ i ]

        MarkNode n ->
            let
                markDefinition =
                    Mark.definition n.mark

                structure =
                    toHtmlNodeFromMarkDefinition markDefinition n.mark childNodesPlaceholder
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
                                    domToEditorInlineLeafTree l (List.drop 1 rest)


{-| Translates a DOM node path to an editor node path. Returns Nothing if the
path is invalid.
-}
domToEditor : BlockNode -> Path -> Maybe Path
domToEditor node path =
    if List.isEmpty path then
        Just []

    else
        let
            parameters =
                elementFromBlockNode node

            nodeDefinition =
                Element.definition parameters

            structure =
                toHtmlNodeFromNodeDefinition nodeDefinition parameters childNodesPlaceholder
        in
        case removePathUpToChildContents structure path of
            Nothing ->
                Nothing

            Just rest ->
                case List.head rest of
                    Nothing ->
                        Just []

                    Just i ->
                        case childNodes node of
                            BlockChildren l ->
                                case Array.get i (fromBlockArray l) of
                                    Nothing ->
                                        Nothing

                                    Just childNode ->
                                        case domToEditor childNode (List.drop 1 rest) of
                                            Nothing ->
                                                Nothing

                                            Just p ->
                                                Just (i :: p)

                            InlineChildren l ->
                                case Array.get i (treeFromInlineArray l) of
                                    Nothing ->
                                        Nothing

                                    Just tree ->
                                        -- we assume the content of the leaf node is valid, but maybe we should validate its content?
                                        domToEditorInlineLeafTree tree (List.drop 1 rest)

                            Leaf ->
                                -- If we still have path left, it means the path is invalid, so we return Nothing
                                Nothing


{-| Translates an editor node path to a DOM node path. Returns Nothing if the
path is invalid.
-}
editorToDom : BlockNode -> Path -> Maybe Path
editorToDom node path =
    case path of
        [] ->
            Just []

        x :: xs ->
            case pathToChildContentsFromElementParameters (elementFromBlockNode node) of
                Nothing ->
                    Nothing

                Just childPath ->
                    case childNodes node of
                        BlockChildren l ->
                            case Array.get x (fromBlockArray l) of
                                Nothing ->
                                    Nothing

                                Just childNode ->
                                    case editorToDom childNode xs of
                                        Nothing ->
                                            Nothing

                                        Just p ->
                                            Just (childPath ++ (x :: p))

                        InlineChildren l ->
                            case Array.get x (reverseLookupFromInlineArray l) of
                                Nothing ->
                                    Nothing

                                Just inlineTreePath ->
                                    case
                                        pathToChildContentsFromInlineTreePath
                                            (fromInlineArray l)
                                            (treeFromInlineArray l)
                                            inlineTreePath
                                    of
                                        Nothing ->
                                            Nothing

                                        Just childInlineTreePath ->
                                            Just (childPath ++ childInlineTreePath)

                        Leaf ->
                            Nothing


increment : Path -> Path
increment np =
    case List.Extra.last np of
        Nothing ->
            []

        Just i ->
            List.take (List.length np - 1) np ++ [ i + 1 ]


decrement : Path -> Path
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


removePathUpToChildContents : HtmlNode -> Path -> Maybe Path
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


pathToChildContents : HtmlNode -> Maybe Path
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


pathToChildContentsFromMark : Mark -> Maybe Path
pathToChildContentsFromMark mark =
    let
        markDefinition =
            Mark.definition mark
    in
    let
        markStructure =
            toHtmlNodeFromMarkDefinition markDefinition mark childNodesPlaceholder
    in
    pathToChildContents markStructure



{- Helper method to determine the path to the child contents from an element editor node -}


pathToChildContentsFromElementParameters : Element -> Maybe Path
pathToChildContentsFromElementParameters parameters =
    let
        nodeDefinition =
            Element.definition parameters

        nodeStructure =
            toHtmlNodeFromNodeDefinition nodeDefinition parameters childNodesPlaceholder
    in
    pathToChildContents nodeStructure


pathToChildContentsFromInlineTreePath : Array InlineLeaf -> Array InlineLeafTree -> Path -> Maybe Path
pathToChildContentsFromInlineTreePath array treeArray path =
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
                            case pathToChildContentsFromMark n.mark of
                                Nothing ->
                                    Nothing

                                Just p ->
                                    case pathToChildContentsFromInlineTreePath array n.children xs of
                                        Nothing ->
                                            Nothing

                                        Just rest ->
                                            Just <| x :: p ++ rest


toString : Path -> String
toString nodePath =
    String.join ":" <| List.map String.fromInt nodePath


parent : Path -> Path
parent =
    RichTextEditor.Model.Node.parent


commonAncestor : Path -> Path -> Path
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
