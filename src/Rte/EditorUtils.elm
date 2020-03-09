module Rte.EditorUtils exposing (..)

import BoundedDeque exposing (BoundedDeque)
import Rte.EditorState exposing (reduceEditorState)
import Rte.Model
    exposing
        ( Command(..)
        , Editor
        , EditorState
        , InternalAction(..)
        , NamedCommand
        , NamedCommandList
        , Transform
        )
import Rte.Spec exposing (validate)


forceRerender : Editor msg -> Editor msg
forceRerender editor =
    { editor | renderCount = editor.renderCount + 1 }


applyInternalCommand : InternalAction -> Editor msg -> Result String (Editor msg)
applyInternalCommand action editor =
    case action of
        Undo ->
            handleUndo editor

        Redo ->
            handleRedo editor


findNextState : EditorState -> BoundedDeque ( String, EditorState ) -> ( Maybe EditorState, BoundedDeque ( String, EditorState ) )
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


handleUndo : Editor msg -> Result String (Editor msg)
handleUndo editor =
    let
        history =
            editor.history

        ( maybeState, newUndoDeque ) =
            findNextState editor.editorState history.undoDeque
    in
    case maybeState of
        Nothing ->
            Err "Cannot undo because there are no different editor states on the undo deque"

        Just newState ->
            let
                newHistory =
                    { history | undoDeque = newUndoDeque, redoStack = editor.editorState :: history.redoStack }
            in
            Ok { editor | editorState = newState, history = newHistory }


handleRedo : Editor msg -> Result String (Editor msg)
handleRedo editor =
    let
        history =
            editor.history
    in
    case editor.history.redoStack of
        [] ->
            Err "There are no states on the redo stack"

        newState :: xs ->
            let
                newHistory =
                    { history
                        | undoDeque =
                            BoundedDeque.pushFront ( "redo", editor.editorState )
                                history.undoDeque
                        , redoStack = xs
                    }
            in
            Ok { editor | editorState = newState, history = newHistory }


updateEditorState : String -> EditorState -> Editor msg -> Editor msg
updateEditorState action editorState editor =
    let
        history =
            editor.history

        newHistory =
            { history
                | undoDeque = BoundedDeque.pushFront ( action, editor.editorState ) history.undoDeque
                , redoStack = []
            }
    in
    { editor | history = newHistory, editorState = editorState }


applyCommand : NamedCommand -> Editor msg -> Result String (Editor msg)
applyCommand ( name, command ) editor =
    case command of
        InternalCommand action ->
            applyInternalCommand action editor

        TransformCommand transform ->
            case transform editor.editorState |> Result.andThen (validate editor.spec) of
                Err s ->
                    Err s

                Ok v ->
                    let
                        reducedState =
                            reduceEditorState v
                    in
                    Ok <| forceReselection (updateEditorState name reducedState editor)


applyNamedCommandList : NamedCommandList -> Editor msg -> Result String (Editor msg)
applyNamedCommandList list editor =
    List.foldl
        (\cmd result ->
            case result of
                Err s ->
                    case applyCommand cmd editor of
                        Err s2 ->
                            let
                                debug =
                                    Debug.log "command failed: " ( cmd, s2 )
                            in
                            Err s

                        Ok o ->
                            Ok o

                _ ->
                    result
        )
        (Err "No commands found")
        list


forceReselection : Editor msg -> Editor msg
forceReselection editor =
    { editor | selectionCount = editor.selectionCount + 1 }
