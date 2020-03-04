module Rte.EditorUtils exposing (..)

import Rte.EditorState exposing (reduceEditorState)
import Rte.Model exposing (Command, Editor, NamedCommand, NamedCommandList)
import Rte.Spec exposing (validate)


zeroWidthSpace =
    "\u{200B}"


forceRerender : Editor msg -> Editor msg
forceRerender editor =
    { editor | renderCount = editor.renderCount + 1 }


applyCommand : NamedCommand -> Editor msg -> Result String (Editor msg)
applyCommand ( name, command ) editor =
    case command editor.editorState |> Result.andThen (validate editor.spec) of
        Err s ->
            Err s

        Ok v ->
            let
                history =
                    editor.history

                -- TODO: use a deque
                newHistory =
                    { history | history = ( name, v ) :: history.history }
            in
            Ok <| forceReselection { editor | history = newHistory, editorState = reduceEditorState v }


applyNamedCommandList : NamedCommandList -> Editor msg -> Result String (Editor msg)
applyNamedCommandList list editor =
    List.foldl
        (\cmd result ->
            case result of
                Err _ ->
                    applyCommand cmd editor

                _ ->
                    result
        )
        (Err "No commands found")
        list


forceReselection : Editor msg -> Editor msg
forceReselection editor =
    { editor | selectionCount = editor.selectionCount + 1 }
