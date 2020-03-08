module Rte.EditorState exposing (..)

import Array exposing (Array)
import List.Extra
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf(..), EditorNode(..), EditorState, Mark, NodePath, inlineLeafArray, selectionAnnotation)
import Rte.Node exposing (findTextBlockNodeAncestor, map)
import Rte.Selection exposing (annotateSelection, clearSelectionAnnotations, rangeSelection)
import Set


removeExtraEmptyTextLeaves : List EditorInlineLeaf -> List EditorInlineLeaf
removeExtraEmptyTextLeaves inlineLeaves =
    case inlineLeaves of
        [] ->
            inlineLeaves

        [ _ ] ->
            inlineLeaves

        x :: y :: xs ->
            case x of
                TextLeaf xL ->
                    case y of
                        TextLeaf yL ->
                            if String.isEmpty xL.text && (not <| Set.member selectionAnnotation xL.annotations) then
                                removeExtraEmptyTextLeaves (y :: xs)

                            else if String.isEmpty yL.text && (not <| Set.member selectionAnnotation yL.annotations) then
                                removeExtraEmptyTextLeaves (x :: xs)

                            else
                                x :: removeExtraEmptyTextLeaves (y :: xs)

                        InlineLeaf _ ->
                            x :: removeExtraEmptyTextLeaves (y :: xs)

                InlineLeaf _ ->
                    x :: removeExtraEmptyTextLeaves (y :: xs)


mergeSimilarInlineLeaves : List EditorInlineLeaf -> List EditorInlineLeaf
mergeSimilarInlineLeaves inlineLeaves =
    case inlineLeaves of
        [] ->
            inlineLeaves

        [ _ ] ->
            inlineLeaves

        x :: y :: xs ->
            case x of
                TextLeaf xL ->
                    case y of
                        TextLeaf yL ->
                            if xL.marks == yL.marks then
                                mergeSimilarInlineLeaves (TextLeaf { xL | text = xL.text ++ yL.text } :: xs)

                            else
                                x :: mergeSimilarInlineLeaves (y :: xs)

                        InlineLeaf _ ->
                            x :: mergeSimilarInlineLeaves (y :: xs)

                InlineLeaf _ ->
                    x :: mergeSimilarInlineLeaves (y :: xs)


reduceNode : EditorBlockNode -> EditorBlockNode
reduceNode node =
    case
        map
            (\x ->
                case x of
                    BlockNodeWrapper bn ->
                        case bn.childNodes of
                            InlineLeafArray a ->
                                BlockNodeWrapper
                                    { bn
                                        | childNodes =
                                            inlineLeafArray <|
                                                Array.fromList
                                                    (mergeSimilarInlineLeaves (removeExtraEmptyTextLeaves (Array.toList a.array)))
                                    }

                            _ ->
                                x

                    _ ->
                        x
            )
            (BlockNodeWrapper node)
    of
        BlockNodeWrapper newNode ->
            newNode

        _ ->
            node


reduceEditorState : EditorState -> EditorState
reduceEditorState editorState =
    let
        markedRoot =
            case editorState.selection of
                Nothing ->
                    editorState.root

                Just selection ->
                    annotateSelection selection editorState.root

        reducedRoot =
            clearSelectionAnnotations <| reduceNode markedRoot
    in
    case editorState.selection of
        Nothing ->
            { editorState | root = reducedRoot }

        Just selection ->
            let
                ( aP, aO ) =
                    translatePath editorState.root reducedRoot selection.anchorNode selection.anchorOffset

                ( fP, fO ) =
                    translatePath editorState.root reducedRoot selection.focusNode selection.focusOffset
            in
            { editorState | root = reducedRoot, selection = Just <| rangeSelection aP aO fP fO }


translatePath : EditorBlockNode -> EditorBlockNode -> NodePath -> Int -> ( NodePath, Int )
translatePath old new path offset =
    case findTextBlockNodeAncestor path old of
        Nothing ->
            ( path, offset )

        Just ( _, oldN ) ->
            case findTextBlockNodeAncestor path new of
                Nothing ->
                    ( path, offset )

                Just ( _, newN ) ->
                    if oldN == newN then
                        ( path, offset )

                    else
                        case oldN.childNodes of
                            InlineLeafArray oldA ->
                                case List.Extra.last path of
                                    Nothing ->
                                        ( path, offset )

                                    Just lastIndex ->
                                        case newN.childNodes of
                                            InlineLeafArray newA ->
                                                let
                                                    pOff =
                                                        parentOffset oldA.array lastIndex offset

                                                    ( cI, cO ) =
                                                        childOffset newA.array pOff

                                                    newPath =
                                                        List.take (List.length path - 1) path ++ [ cI ]
                                                in
                                                ( newPath, cO )

                                            _ ->
                                                ( path, offset )

                            _ ->
                                ( path, offset )


parentOffset : Array EditorInlineLeaf -> Int -> Int -> Int
parentOffset leaves index offset =
    let
        ( _, newOffset ) =
            Array.foldl
                (\l ( i, accOffset ) ->
                    case l of
                        TextLeaf tl ->
                            ( i + 1
                            , if i < index then
                                accOffset + String.length tl.text

                              else
                                accOffset
                            )

                        InlineLeaf _ ->
                            ( i + 1
                            , if i < index then
                                accOffset + 1

                              else
                                accOffset
                            )
                )
                ( 0, offset )
                leaves
    in
    newOffset


childOffset : Array EditorInlineLeaf -> Int -> ( Int, Int )
childOffset leaves offset =
    let
        ( newIndex, newOffset, _ ) =
            Array.foldl
                (\l ( i, accOffset, done ) ->
                    if done then
                        ( i, accOffset, done )

                    else if accOffset <= 0 then
                        ( i, accOffset, True )

                    else
                        case l of
                            TextLeaf tl ->
                                if accOffset <= String.length tl.text then
                                    ( i, accOffset, True )

                                else
                                    ( i + 1, accOffset - String.length tl.text, False )

                            InlineLeaf _ ->
                                ( i + 1, accOffset - 1, False )
                )
                ( 0, offset, False )
                leaves
    in
    ( newIndex, newOffset )
