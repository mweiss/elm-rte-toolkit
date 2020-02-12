module Rte.NodePath exposing (..)

import List.Extra
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf(..), ElementParameters, HtmlNode(..), Mark, NodePath, Selection, Spec)
import Rte.Spec exposing (childNodesPlaceholder, findMarkDefinitionsFromSpec, findNodeDefinitionFromSpec)


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
                        case List.Extra.getAt x children of
                            Nothing ->
                                Nothing

                            Just child ->
                                removePathUpToChildContents child xs


pathToChildContents : HtmlNode -> Maybe NodePath
pathToChildContents node =
    case node of
        ElementNode _ _ children ->
            if children == childNodesPlaceholder then
                Just []

            else
                List.foldl
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
                    (List.indexedMap Tuple.pair children)


domToEditor : Spec -> EditorBlockNode -> NodePath -> Maybe NodePath
domToEditor spec node path =
    if List.isEmpty path then
        Just []

    else
        let
            nodeDefinition =
                findNodeDefinitionFromSpec node.parameters.name spec

            markDefinitions =
                findMarkDefinitionsFromSpec node.parameters.marks spec

            structure =
                nodeDefinition.toHtmlNode node.parameters

            maybePathWithMarksRemoved =
                List.foldl
                    (\( mark, markDefinition ) maybePath ->
                        case maybePath of
                            Nothing ->
                                Nothing

                            Just pathSoFar ->
                                let
                                    markStructure =
                                        markDefinition.toHtmlNode mark
                                in
                                removePathUpToChildContents markStructure pathSoFar
                    )
                    (Just path)
                    markDefinitions
        in
        case maybePathWithMarksRemoved of
            Nothing ->
                Nothing

            Just pathWithMarksRemoved ->
                case removePathUpToChildContents structure path of
                    Nothing ->
                        Nothing

                    Just rest ->
                        case List.head rest of
                            Nothing ->
                                Just []

                            Just i ->
                                case node.childNodes of
                                    BlockList l ->
                                        case List.Extra.getAt i l of
                                            Nothing ->
                                                Nothing

                                            Just childNode ->
                                                case domToEditor spec childNode (List.drop 1 rest) of
                                                    Nothing ->
                                                        Nothing

                                                    Just p ->
                                                        Just (i :: p)

                                    InlineLeafList l ->
                                        case List.Extra.getAt i l of
                                            Nothing ->
                                                Nothing

                                            Just _ ->
                                                --  TODO:  we assume the content of the leaf node is valid, but maybe we should validate its content?
                                                Just [ i ]

                                    Leaf ->
                                        -- If we still have path left, it means the path is invalid, so we return Nothing
                                        Nothing


pathToChildContentsFromMarks : Spec -> List Mark -> Maybe NodePath
pathToChildContentsFromMarks spec marks =
    let
        markDefinitions =
            findMarkDefinitionsFromSpec marks spec
    in
    List.foldl
        (\( mark, markDefinition ) maybePath ->
            case maybePath of
                Nothing ->
                    Nothing

                Just pathSoFar ->
                    let
                        markStructure =
                            markDefinition.toHtmlNode mark
                    in
                    case pathToChildContents markStructure of
                        Nothing ->
                            Nothing

                        Just p ->
                            Just (pathSoFar ++ p)
        )
        (Just [])
        markDefinitions


pathToChildContentsFromElementParameters : Spec -> ElementParameters -> Maybe NodePath
pathToChildContentsFromElementParameters spec parameters =
    let
        nodeDefinition =
            findNodeDefinitionFromSpec parameters.name spec

        nodeStructure =
            nodeDefinition.toHtmlNode parameters

        maybePathToChildContents =
            pathToChildContentsFromMarks spec parameters.marks
    in
    case maybePathToChildContents of
        Nothing ->
            Nothing

        Just markPath ->
            case pathToChildContents nodeStructure of
                Nothing ->
                    Nothing

                Just nodePath ->
                    Just <| markPath ++ nodePath


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
                        BlockList l ->
                            case List.Extra.getAt x l of
                                Nothing ->
                                    Nothing

                                Just childNode ->
                                    case editorToDom spec childNode xs of
                                        Nothing ->
                                            Nothing

                                        Just p ->
                                            Just (childPath ++ (x :: p))

                        InlineLeafList l ->
                            case List.Extra.getAt x l of
                                Nothing ->
                                    Nothing

                                Just childNode ->
                                    case childNode of
                                        TextLeaf contents ->
                                            case pathToChildContentsFromMarks spec contents.marks of
                                                Nothing ->
                                                    Nothing

                                                Just childMarkPath ->
                                                    Just (childPath ++ childMarkPath)

                                        InlineLeaf contents ->
                                            case pathToChildContentsFromElementParameters spec contents of
                                                Nothing ->
                                                    Nothing

                                                Just childNodePath ->
                                                    Just (childPath ++ childNodePath)

                        Leaf ->
                            Nothing
