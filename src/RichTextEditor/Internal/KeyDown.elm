module RichTextEditor.Internal.KeyDown exposing (..)

import Json.Decode as D
import RichTextEditor.Internal.Editor exposing (applyNamedCommandList)
import RichTextEditor.Model.Command exposing (namedCommandListFromKeyboardEvent)
import RichTextEditor.Model.Editor exposing (DecoderFunc, Editor, InternalEditorMsg(..), commandMap)
import RichTextEditor.Model.Event exposing (KeyboardEvent)


preventDefaultOn : Editor -> InternalEditorMsg -> ( InternalEditorMsg, Bool )
preventDefaultOn editor msg =
    case msg of
        KeyDownEvent key ->
            ( msg, shouldPreventDefault editor key )

        _ ->
            ( msg, False )


shouldPreventDefault : Editor -> KeyboardEvent -> Bool
shouldPreventDefault editor keyboardEvent =
    case handleKeyDownEvent editor keyboardEvent of
        Err _ ->
            False

        Ok _ ->
            True


preventDefaultOnKeyDownDecoder : DecoderFunc msg -> Editor -> D.Decoder ( msg, Bool )
preventDefaultOnKeyDownDecoder decoder editor =
    D.map (\( i, b ) -> ( decoder i, b )) (D.map (preventDefaultOn editor) keyDownDecoder)


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


handleKeyDownEvent : Editor -> KeyboardEvent -> Result String Editor
handleKeyDownEvent editor event =
    let
        namedCommandList =
            namedCommandListFromKeyboardEvent event (commandMap editor)
    in
    applyNamedCommandList namedCommandList editor


handleKeyDown : KeyboardEvent -> Editor -> Editor
handleKeyDown keyboardEvent editor =
    Result.withDefault editor <| handleKeyDownEvent editor keyboardEvent
