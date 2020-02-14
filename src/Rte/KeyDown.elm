module Rte.KeyDown exposing (..)

import Dict
import Json.Decode as D
import Rte.Commands exposing (altKey, ctrlKey, metaKey, shiftKey)
import Rte.Model exposing (Editor, EditorState, InputEvent, InternalEditorMsg(..), KeyMap, KeyboardEvent)


preventDefaultOn : KeyMap -> EditorState -> InternalEditorMsg -> ( InternalEditorMsg, Bool )
preventDefaultOn keyMap editorState msg =
    case msg of
        KeyDownEvent key ->
            ( msg, shouldPreventDefault keyMap editorState key )

        _ ->
            ( msg, False )


shouldPreventDefault : KeyMap -> EditorState -> KeyboardEvent -> Bool
shouldPreventDefault keyMap editorState keyboardEvent =
    case handleKeyDownEvent keyMap editorState keyboardEvent of
        Nothing ->
            False

        Just _ ->
            True


preventDefaultOnKeyDownDecoder : KeyMap -> EditorState -> (InternalEditorMsg -> msg) -> D.Decoder ( msg, Bool )
preventDefaultOnKeyDownDecoder keyMap editorState msgFunc =
    D.map (\( i, b ) -> ( msgFunc i, b )) (D.map (preventDefaultOn keyMap editorState) keyDownDecoder)


keyDownDecoder : D.Decoder InternalEditorMsg
keyDownDecoder =
    D.map KeyDownEvent <|
        D.map7 KeyboardEvent
            (D.field "keyCode" D.int)
            (D.field "key" D.string)
            (D.field "altKey" D.bool)
            (D.field "metaKey" D.bool)
            (D.field "ctrlKey" D.bool)
            (D.field "shiftKey" D.bool)
            (D.oneOf [ D.field "isComposing" D.bool, D.succeed False ])


keyboardEventToDictKey : KeyboardEvent -> List String
keyboardEventToDictKey keyboardEvent =
    List.sort
        ([ keyboardEvent.key ]
            |> addShiftKey keyboardEvent
            |> addMetaKey keyboardEvent
            |> addCtrlKey keyboardEvent
            |> addAltKey keyboardEvent
        )


addShiftKey : KeyboardEvent -> List String -> List String
addShiftKey keyboardEvent keys =
    if keyboardEvent.shiftKey then
        shiftKey :: keys

    else
        keys


addMetaKey : KeyboardEvent -> List String -> List String
addMetaKey keyboardEvent keys =
    if keyboardEvent.metaKey then
        metaKey :: keys

    else
        keys


addCtrlKey : KeyboardEvent -> List String -> List String
addCtrlKey keyboardEvent keys =
    if keyboardEvent.ctrlKey then
        ctrlKey :: keys

    else
        keys


addAltKey : KeyboardEvent -> List String -> List String
addAltKey keyboardEvent keys =
    if keyboardEvent.altKey then
        altKey :: keys

    else
        keys


handleKeyDownEvent : KeyMap -> EditorState -> KeyboardEvent -> Maybe EditorState
handleKeyDownEvent keyMap editorState keyboardEvent =
    case Dict.get (keyboardEventToDictKey keyboardEvent) keyMap of
        Nothing ->
            Nothing

        Just command ->
            case command editorState of
                Err _ ->
                    Nothing

                Ok v ->
                    Just v


handleKeyDown : KeyboardEvent -> Editor msg -> Editor msg
handleKeyDown keyboardEvent editor =
    case handleKeyDownEvent editor.commandMap.keyMap editor.editorState keyboardEvent of
        Nothing ->
            editor

        Just editorState ->
            { editor | editorState = editorState }
