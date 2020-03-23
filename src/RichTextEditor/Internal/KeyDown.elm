module RichTextEditor.Internal.KeyDown exposing (..)

import Json.Decode as D
import RichTextEditor.Internal.Editor exposing (applyNamedCommandList)
import RichTextEditor.Model.Command exposing (CommandMap, namedCommandListFromKeyboardEvent)
import RichTextEditor.Model.Editor exposing (Editor, InternalEditorMsg(..), Tagger, shortKey)
import RichTextEditor.Model.Event exposing (KeyboardEvent)
import RichTextEditor.Model.Spec exposing (Spec)


preventDefaultOn : CommandMap -> Spec -> Editor -> InternalEditorMsg -> ( InternalEditorMsg, Bool )
preventDefaultOn commandMap spec editor msg =
    case msg of
        KeyDownEvent key ->
            ( msg, shouldPreventDefault commandMap spec editor key )

        _ ->
            ( msg, False )


shouldPreventDefault : CommandMap -> Spec -> Editor -> KeyboardEvent -> Bool
shouldPreventDefault comamndMap spec editor keyboardEvent =
    case handleKeyDownEvent comamndMap spec editor keyboardEvent of
        Err _ ->
            False

        Ok _ ->
            True


preventDefaultOnKeyDownDecoder : Tagger msg -> CommandMap -> Spec -> Editor -> D.Decoder ( msg, Bool )
preventDefaultOnKeyDownDecoder tagger commandMap spec editor =
    D.map (\( i, b ) -> ( tagger i, b )) (D.map (preventDefaultOn commandMap spec editor) keyDownDecoder)


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


handleKeyDownEvent : CommandMap -> Spec -> Editor -> KeyboardEvent -> Result String Editor
handleKeyDownEvent commandMap spec editor event =
    let
        namedCommandList =
            namedCommandListFromKeyboardEvent (shortKey editor) event commandMap
    in
    applyNamedCommandList namedCommandList spec editor


handleKeyDown : KeyboardEvent -> CommandMap -> Spec -> Editor -> Editor
handleKeyDown keyboardEvent commandMap spec editor =
    Result.withDefault editor <| handleKeyDownEvent commandMap spec editor keyboardEvent
