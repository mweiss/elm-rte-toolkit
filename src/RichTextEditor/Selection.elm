module RichTextEditor.Selection exposing
    ( annotateSelection
    , clearSelectionAnnotations
    , domToEditor
    , editorToDom
    , selectionFromAnnotations
    )

import RichTextEditor.Annotation
    exposing
        ( addAnnotationAtPath
        , clearAnnotations
        , findPathsWithAnnotation
        )
import RichTextEditor.Model.Annotations exposing (selection)
import RichTextEditor.Model.Node exposing (Block, Path)
import RichTextEditor.Model.Selection
    exposing
        ( Selection
        , anchorNode
        , anchorOffset
        , focusNode
        , focusOffset
        , rangeSelection
        )
import RichTextEditor.NodePath as Path


domToEditor : Block -> Selection -> Maybe Selection
domToEditor =
    transformSelection Path.domToEditor


editorToDom : Block -> Selection -> Maybe Selection
editorToDom =
    transformSelection Path.editorToDom


transformSelection : (Block -> Path -> Maybe Path) -> Block -> Selection -> Maybe Selection
transformSelection transformation node selection =
    case transformation node (anchorNode selection) of
        Nothing ->
            Nothing

        Just an ->
            case transformation node (focusNode selection) of
                Nothing ->
                    Nothing

                Just fn ->
                    Just <| rangeSelection an (anchorOffset selection) fn (focusOffset selection)


annotateSelection : Selection -> Block -> Block
annotateSelection selection node =
    addSelectionAnnotationAtPath (focusNode selection) <| addSelectionAnnotationAtPath (anchorNode selection) node


addSelectionAnnotationAtPath : Path -> Block -> Block
addSelectionAnnotationAtPath nodePath node =
    Result.withDefault node (addAnnotationAtPath selection nodePath node)


clearSelectionAnnotations : Block -> Block
clearSelectionAnnotations =
    clearAnnotations selection


selectionFromAnnotations : Block -> Int -> Int -> Maybe Selection
selectionFromAnnotations node anchorOffset focusOffset =
    case findNodeRangeFromSelectionAnnotations node of
        Nothing ->
            Nothing

        Just ( start, end ) ->
            Just (rangeSelection start anchorOffset end focusOffset)


findNodeRangeFromSelectionAnnotations : Block -> Maybe ( Path, Path )
findNodeRangeFromSelectionAnnotations node =
    let
        paths =
            findPathsWithAnnotation selection node
    in
    case paths of
        [] ->
            Nothing

        [ x ] ->
            Just ( x, x )

        end :: start :: _ ->
            Just ( start, end )
