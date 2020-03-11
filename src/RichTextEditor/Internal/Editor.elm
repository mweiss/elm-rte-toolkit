module RichTextEditor.Internal.Editor exposing (..)

import BoundedDeque exposing (BoundedDeque)
import RichTextEditor.EditorState exposing (reduceEditorState)
import RichTextEditor.Model.Command exposing (Command(..), InternalAction(..), NamedCommand, NamedCommandList)
import RichTextEditor.Model.Editor exposing (Editor, forceReselection, history, spec, state, withHistory, withState)
import RichTextEditor.Model.History exposing (contents, fromContents)
import RichTextEditor.Model.State exposing (State)
import RichTextEditor.Spec exposing (validate)


applyInternalCommand : InternalAction -> Editor msg -> Result String (Editor msg)
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


handleUndo : Editor msg -> Result String (Editor msg)
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


handleRedo : Editor msg -> Result String (Editor msg)
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


updateEditorState : String -> State -> Editor msg -> Editor msg
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


applyCommand : NamedCommand -> Editor msg -> Result String (Editor msg)
applyCommand ( name, command ) editor =
    case command of
        InternalCommand action ->
            applyInternalCommand action editor

        TransformCommand transform ->
            case transform (state editor) |> Result.andThen (validate (spec editor)) of
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
