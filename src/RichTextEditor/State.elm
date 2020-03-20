module RichTextEditor.State exposing (reduceEditorState)

import Array exposing (Array)
import List.Extra
import RichTextEditor.Model.Annotations exposing (selection)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , ChildNodes(..)
        , InlineLeaf(..)
        , InlineLeafArray
        , Node(..)
        , Path
        , annotationsFromTextLeafParameters
        , childNodes
        , comparableMarksFromTextLeafParameters
        , fromInlineArray
        , inlineLeafArray
        , isSameBlockNode
        , text
        , withChildNodes
        , withText
        )
import RichTextEditor.Model.Selection
    exposing
        ( anchorNode
        , anchorOffset
        , focusNode
        , focusOffset
        , rangeSelection
        )
import RichTextEditor.Model.State as State exposing (State, withRoot, withSelection)
import RichTextEditor.Node exposing (findTextBlockNodeAncestor, map)
import RichTextEditor.Selection exposing (annotateSelection, clearSelectionAnnotations)
import Set


removeExtraEmptyTextLeaves : List InlineLeaf -> List InlineLeaf
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
                            if String.isEmpty (text xL) && (not <| Set.member selection (annotationsFromTextLeafParameters xL)) then
                                removeExtraEmptyTextLeaves (y :: xs)

                            else if String.isEmpty (text yL) && (not <| Set.member selection (annotationsFromTextLeafParameters yL)) then
                                removeExtraEmptyTextLeaves (x :: xs)

                            else
                                x :: removeExtraEmptyTextLeaves (y :: xs)

                        InlineLeaf _ ->
                            x :: removeExtraEmptyTextLeaves (y :: xs)

                InlineLeaf _ ->
                    x :: removeExtraEmptyTextLeaves (y :: xs)


mergeSimilarInlineLeaves : List InlineLeaf -> List InlineLeaf
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
                            if comparableMarksFromTextLeafParameters xL == comparableMarksFromTextLeafParameters yL then
                                mergeSimilarInlineLeaves (TextLeaf (xL |> withText (text xL ++ text yL)) :: xs)

                            else
                                x :: mergeSimilarInlineLeaves (y :: xs)

                        InlineLeaf _ ->
                            x :: mergeSimilarInlineLeaves (y :: xs)

                InlineLeaf _ ->
                    x :: mergeSimilarInlineLeaves (y :: xs)


reduceNode : BlockNode -> BlockNode
reduceNode node =
    case
        map
            (\x ->
                case x of
                    Block bn ->
                        case childNodes bn of
                            InlineChildren a ->
                                Block <|
                                    (bn
                                        |> withChildNodes
                                            (inlineLeafArray <|
                                                Array.fromList
                                                    (mergeSimilarInlineLeaves (removeExtraEmptyTextLeaves (Array.toList (fromInlineArray a))))
                                            )
                                    )

                            _ ->
                                x

                    _ ->
                        x
            )
            (Block node)
    of
        Block newNode ->
            newNode

        _ ->
            node


reduceEditorState : State -> State
reduceEditorState editorState =
    let
        markedRoot =
            case State.selection editorState of
                Nothing ->
                    State.root editorState

                Just selection ->
                    annotateSelection selection (State.root editorState)

        reducedRoot =
            clearSelectionAnnotations <| reduceNode markedRoot
    in
    case State.selection editorState of
        Nothing ->
            editorState |> withRoot reducedRoot

        Just selection ->
            let
                ( aP, aO ) =
                    translatePath (State.root editorState) reducedRoot (anchorNode selection) (anchorOffset selection)

                ( fP, fO ) =
                    translatePath (State.root editorState) reducedRoot (focusNode selection) (focusOffset selection)
            in
            editorState
                |> withRoot reducedRoot
                |> withSelection (Just <| rangeSelection aP aO fP fO)


translatePath : BlockNode -> BlockNode -> Path -> Int -> ( Path, Int )
translatePath old new path offset =
    case findTextBlockNodeAncestor path old of
        Nothing ->
            ( path, offset )

        Just ( _, oldN ) ->
            case findTextBlockNodeAncestor path new of
                Nothing ->
                    ( path, offset )

                Just ( _, newN ) ->
                    if isSameBlockNode oldN newN then
                        ( path, offset )

                    else
                        case childNodes oldN of
                            InlineChildren oldA ->
                                case List.Extra.last path of
                                    Nothing ->
                                        ( path, offset )

                                    Just lastIndex ->
                                        case childNodes newN of
                                            InlineChildren newA ->
                                                let
                                                    pOff =
                                                        parentOffset (fromInlineArray oldA) lastIndex offset

                                                    ( cI, cO ) =
                                                        childOffset (fromInlineArray newA) pOff

                                                    newPath =
                                                        List.take (List.length path - 1) path ++ [ cI ]
                                                in
                                                ( newPath, cO )

                                            _ ->
                                                ( path, offset )

                            _ ->
                                ( path, offset )


parentOffset : Array InlineLeaf -> Int -> Int -> Int
parentOffset leaves index offset =
    let
        ( _, newOffset ) =
            Array.foldl
                (\l ( i, accOffset ) ->
                    case l of
                        TextLeaf tl ->
                            ( i + 1
                            , if i < index then
                                accOffset + String.length (text tl)

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


childOffset : Array InlineLeaf -> Int -> ( Int, Int )
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
                                if accOffset <= String.length (text tl) then
                                    ( i, accOffset, True )

                                else
                                    ( i + 1, accOffset - String.length (text tl), False )

                            InlineLeaf _ ->
                                ( i + 1, accOffset - 1, False )
                )
                ( 0, offset, False )
                leaves
    in
    ( newIndex, newOffset )
