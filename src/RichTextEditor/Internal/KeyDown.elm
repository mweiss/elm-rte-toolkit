module RichTextEditor.KeyDown exposing (..)

import Dict
import Json.Decode as D
import RichTextEditor.Commands exposing (altKey, ctrlKey, metaKey, shiftKey)
import RichTextEditor.Editor exposing (applyNamedCommandList)
import RichTextEditor.Internal.Model
    exposing
        ( Editor
        , EditorState
        , InputEvent
        , InternalEditorMsg(..)
        , KeyMap
        , KeyboardEvent
        )


preventDefaultOn : Editor msg -> InternalEditorMsg -> ( InternalEditorMsg, Bool )
preventDefaultOn editor msg =
    case msg of
        KeyDownEvent key ->
            ( msg, shouldPreventDefault editor key )

        _ ->
            ( msg, False )


shouldPreventDefault : Editor msg -> KeyboardEvent -> Bool
shouldPreventDefault editor keyboardEvent =
    case handleKeyDownEvent editor keyboardEvent of
        Err _ ->
            False

        Ok _ ->
            True


preventDefaultOnKeyDownDecoder : Editor msg -> D.Decoder ( msg, Bool )
preventDefaultOnKeyDownDecoder editor =
    D.map (\( i, b ) -> ( editor.decoder i, b )) (D.map (preventDefaultOn editor) keyDownDecoder)


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


handleKeyDownEvent : Editor msg -> KeyboardEvent -> Result String (Editor msg)
handleKeyDownEvent editor keyboardEvent =
    let
        namedCommandList =
            Maybe.withDefault (editor.commandMap.defaultKeyCommand keyboardEvent)
                (Dict.get (keyboardEventToDictKey keyboardEvent) editor.commandMap.keyMap)
    in
    applyNamedCommandList namedCommandList editor


handleKeyDown : KeyboardEvent -> Editor msg -> Editor msg
handleKeyDown keyboardEvent editor =
    Result.withDefault editor <| handleKeyDownEvent editor keyboardEvent
