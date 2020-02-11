module Rte.BeforeInput exposing (..)

import Dict
import Json.Decode as D
import Rte.EditorUtils exposing (forceRerender)
import Rte.Model exposing (Editor, EditorState, InputEvent, InputEventTypeMap, InternalEditorMsg(..))


preventDefaultOn : InputEventTypeMap -> EditorState -> InternalEditorMsg -> ( InternalEditorMsg, Bool )
preventDefaultOn inputEventTypeMap editorState msg =
    case msg of
        BeforeInputEvent inputEvent ->
            ( msg, shouldPreventDefault inputEventTypeMap editorState inputEvent.inputType )

        _ ->
            ( msg, False )


shouldPreventDefault : InputEventTypeMap -> EditorState -> String -> Bool
shouldPreventDefault inputEventTypeMap editorState inputType =
    case handleInputEvent inputEventTypeMap editorState inputType of
        Nothing ->
            False

        Just _ ->
            True


preventDefaultOnBeforeInputDecoder : InputEventTypeMap -> EditorState -> (InternalEditorMsg -> msg) -> D.Decoder ( msg, Bool )
preventDefaultOnBeforeInputDecoder inputEventTypeMap editorState msgFunc =
    D.map (\( i, b ) -> ( msgFunc i, b )) (D.map (preventDefaultOn inputEventTypeMap editorState) beforeInputDecoder)


beforeInputDecoder : D.Decoder InternalEditorMsg
beforeInputDecoder =
    D.map BeforeInputEvent
        (D.map3 InputEvent
            (D.maybe (D.field "data" D.string))
            (D.oneOf [ D.field "isComposing" D.bool, D.succeed False ])
            (D.field "inputType" D.string)
        )


handleInputEvent : InputEventTypeMap -> EditorState -> String -> Maybe EditorState
handleInputEvent inputEventTypeMap editorState inputType =
    case Dict.get inputType inputEventTypeMap of
        Nothing ->
            Nothing

        Just command ->
            command editorState


handleBeforeInput : InputEvent -> Editor msg -> Editor msg
handleBeforeInput inputEvent editor =
    case handleInputEvent editor.commandMap.inputEventTypeMap editor.editorState inputEvent.inputType of
        Nothing ->
            editor

        Just editorState ->
            -- HACK: Android has very strange behavior with regards to before input events, so
            -- we're forcing a rerender if we update the editor state on a command
            forceRerender { editor | editorState = editorState }
