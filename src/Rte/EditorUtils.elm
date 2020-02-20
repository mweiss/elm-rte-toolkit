module Rte.EditorUtils exposing (..)

import Rte.EditorState exposing (reduceEditorState)
import Rte.Model exposing (CommandFunc, Editor)


zeroWidthSpace =
    "\u{200B}"


forceRerender : Editor msg -> Editor msg
forceRerender editor =
    { editor | renderCount = editor.renderCount + 1 }


applyCommand : CommandFunc -> Editor msg -> Result String (Editor msg)
applyCommand command editor =
    case command editor.editorState of
        Err s ->
            Err s

        Ok v ->
            Ok <| forceReselection { editor | editorState = reduceEditorState v }


forceReselection : Editor msg -> Editor msg
forceReselection editor =
    { editor | selectionCount = editor.selectionCount + 1 }
