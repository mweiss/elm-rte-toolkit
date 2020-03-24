module RichTextEditor.Internal.Editor exposing (..)

import BoundedDeque exposing (BoundedDeque)
import RichTextEditor.Config.Command
    exposing
        ( Command(..)
        , InternalAction(..)
        , NamedCommand
        , NamedCommandList
        )
import RichTextEditor.Config.Spec exposing (Spec)
import RichTextEditor.Internal.Model.Editor
    exposing
        ( Editor
        , forceReselection
        , history
        , state
        , withHistory
        , withState
        )
import RichTextEditor.Model.History exposing (contents, fromContents)
import RichTextEditor.Model.State exposing (State)
import RichTextEditor.State exposing (reduceEditorState, validate)


applyInternalCommand : InternalAction -> Editor -> Result String Editor
applyInternalCommand action editor =
    case action of
        Undo ->
            handleUndo editor

        Redo ->
            handleRedo editor


findNextState : State -> BoundedDeque ( String, State ) -> ( Maybe State, BoundedDeque ( String, State ) )
findNextState editorState undoDeque =
    let
        ( maybeState, rest ) =
            BoundedDeque.popFront undoDeque
    in
    case maybeState of
        Nothing ->
            ( Nothing, rest )

        Just ( _, state ) ->
            if state /= editorState then
                ( Just state, rest )

            else
                findNextState editorState rest


handleUndo : Editor -> Result String Editor
handleUndo editor =
    let
        editorHistory =
            contents (history editor)

        editorState =
            state editor

        ( maybeState, newUndoDeque ) =
            findNextState editorState editorHistory.undoDeque
    in
    case maybeState of
        Nothing ->
            Err "Cannot undo because there are no different editor states on the undo deque"

        Just newState ->
            let
                newHistory =
                    { editorHistory | undoDeque = newUndoDeque, redoStack = editorState :: editorHistory.redoStack }
            in
            Ok (editor |> withState newState |> withHistory (fromContents newHistory))


handleRedo : Editor -> Result String Editor
handleRedo editor =
    let
        editorHistory =
            contents (history editor)
    in
    case editorHistory.redoStack of
        [] ->
            Err "There are no states on the redo stack"

        newState :: xs ->
            let
                newHistory =
                    { editorHistory
                        | undoDeque =
                            BoundedDeque.pushFront ( "redo", state editor )
                                editorHistory.undoDeque
                        , redoStack = xs
                    }
            in
            Ok (editor |> withState newState |> withHistory (fromContents newHistory))


updateEditorState : String -> State -> Editor -> Editor
updateEditorState action newState editor =
    let
        editorHistory =
            contents (history editor)

        newHistory =
            { editorHistory
                | undoDeque = BoundedDeque.pushFront ( action, state editor ) editorHistory.undoDeque
                , redoStack = []
            }
    in
    editor |> withState newState |> withHistory (fromContents newHistory)


applyCommand : NamedCommand -> Spec -> Editor -> Result String Editor
applyCommand ( name, command ) spec editor =
    case command of
        InternalCommand action ->
            applyInternalCommand action editor

        TransformCommand transform ->
            case transform (state editor) |> Result.andThen (validate spec) of
                Err s ->
                    Err s

                Ok v ->
                    let
                        reducedState =
                            reduceEditorState v
                    in
                    Ok <| forceReselection (updateEditorState name reducedState editor)


applyCommandNoForceSelection : NamedCommand -> Spec -> Editor -> Result String Editor
applyCommandNoForceSelection ( name, command ) spec editor =
    case command of
        InternalCommand action ->
            applyInternalCommand action editor

        TransformCommand transform ->
            case transform (state editor) |> Result.andThen (validate spec) of
                Err s ->
                    Err s

                Ok v ->
                    let
                        reducedState =
                            reduceEditorState v
                    in
                    Ok <| updateEditorState name reducedState editor


applyNamedCommandList : NamedCommandList -> Spec -> Editor -> Result String Editor
applyNamedCommandList list spec editor =
    List.foldl
        (\cmd result ->
            case result of
                Err _ ->
                    case applyCommand cmd spec editor of
                        Err s2 ->
                            Err s2

                        Ok o ->
                            Ok o

                _ ->
                    result
        )
        (Err "No commands found")
        list
