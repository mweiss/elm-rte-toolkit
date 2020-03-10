module RichTextEditor.Selection exposing
    ( annotateSelection
    , caretSelection
    , clearSelectionAnnotations
    , domToEditor
    , editorToDom
    , isCollapsed
    , normalizeSelection
    , rangeSelection
    , selectionFromAnnotations
    , singleNodeRangeSelection
    )

import RichTextEditor.Annotation
    exposing
        ( addAnnotationAtPath
        , clearAnnotations
        , findPathsWithAnnotation
        )
import RichTextEditor.Model
    exposing
        ( Annotation
        , ChildNodes(..)
        , EditorBlockNode
        , EditorInlineLeaf(..)
        , EditorNode(..)
        , ElementParameters
        , HtmlNode(..)
        , Mark
        , NodePath
        , Selection
        , Spec
        , selectionAnnotation
        )
import RichTextEditor.NodePath as Path


domToEditor : Spec -> EditorBlockNode -> Selection -> Maybe Selection
domToEditor =
    transformSelection Path.domToEditor


editorToDom : Spec -> EditorBlockNode -> Selection -> Maybe Selection
editorToDom =
    transformSelection Path.editorToDom


transformSelection : (Spec -> EditorBlockNode -> NodePath -> Maybe NodePath) -> Spec -> EditorBlockNode -> Selection -> Maybe Selection
transformSelection transformation spec node selection =
    case transformation spec node selection.anchorNode of
        Nothing ->
            Nothing

        Just anchorNode ->
            case transformation spec node selection.focusNode of
                Nothing ->
                    Nothing

                Just focusNode ->
                    Just <| rangeSelection anchorNode selection.anchorOffset focusNode selection.focusOffset


annotateSelection : Selection -> EditorBlockNode -> EditorBlockNode
annotateSelection selection node =
    addSelectionAnnotationAtPath selection.focusNode <| addSelectionAnnotationAtPath selection.anchorNode node


addSelectionAnnotationAtPath : NodePath -> EditorBlockNode -> EditorBlockNode
addSelectionAnnotationAtPath nodePath node =
    Result.withDefault node (addAnnotationAtPath selectionAnnotation nodePath node)


clearSelectionAnnotations : EditorBlockNode -> EditorBlockNode
clearSelectionAnnotations =
    clearAnnotations selectionAnnotation


selectionFromAnnotations : EditorBlockNode -> Int -> Int -> Maybe Selection
selectionFromAnnotations node anchorOffset focusOffset =
    case findNodeRangeFromSelectionAnnotations node of
        Nothing ->
            Nothing

        Just ( start, end ) ->
            Just (rangeSelection start anchorOffset end focusOffset)


findNodeRangeFromSelectionAnnotations : EditorBlockNode -> Maybe ( NodePath, NodePath )
findNodeRangeFromSelectionAnnotations node =
    let
        paths =
            findPathsWithAnnotation selectionAnnotation node
    in
    case paths of
        [] ->
            Nothing

        [ x ] ->
            Just ( x, x )

        end :: start :: _ ->
            Just ( start, end )
