module Rte.EditorState exposing (..)

import Array exposing (Array)
import List.Extra
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf(..), EditorState, Mark, NodePath, selectionMark)
import Rte.Node exposing (EditorNode(..), findTextBlockNodeAncestor, map)
import Rte.Selection exposing (clearSelectionMarks, markSelection, rangeSelection)


removeExtraEmptyTextLeaves : List EditorInlineLeaf -> List EditorInlineLeaf
removeExtraEmptyTextLeaves inlineLeaves =
    Debug.log "removeExtraEmptyTextLeaves" <|
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
                                if String.isEmpty xL.text && (not <| List.member selectionMark xL.marks) then
                                    removeExtraEmptyTextLeaves (y :: xs)

                                else if String.isEmpty yL.text && (not <| List.member selectionMark yL.marks) then
                                    removeExtraEmptyTextLeaves (x :: xs)

                                else
                                    x :: removeExtraEmptyTextLeaves (y :: xs)

                            InlineLeaf _ ->
                                x :: removeExtraEmptyTextLeaves (y :: xs)

                    InlineLeaf _ ->
                        x :: removeExtraEmptyTextLeaves (y :: xs)


filterSelectionMark : List Mark -> List Mark
filterSelectionMark marks =
    List.filter (\m -> m /= selectionMark) marks


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
                            if filterSelectionMark xL.marks == filterSelectionMark yL.marks then
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
                                            InlineLeafArray <|
                                                Array.fromList
                                                    (mergeSimilarInlineLeaves (removeExtraEmptyTextLeaves (Array.toList a)))
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
                    markSelection selection editorState.root

        reducedRoot =
            clearSelectionMarks <| reduceNode markedRoot
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

        Just ( oldP, oldN ) ->
            case findTextBlockNodeAncestor path new of
                Nothing ->
                    ( path, offset )

                Just ( newP, newN ) ->
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
                                                        parentOffset oldA lastIndex offset

                                                    ( cI, cO ) =
                                                        childOffset newA pOff

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
