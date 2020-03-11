module RichTextEditor.Internal.BeforeInput exposing (..)

import Json.Decode as D
import RichTextEditor.Internal.Editor exposing (applyNamedCommandList)
import RichTextEditor.Model.Command exposing (namedCommandListFromInputEvent)
import RichTextEditor.Model.Editor exposing (Editor, InternalEditorMsg(..), commandMap, decoder, forceRerender)
import RichTextEditor.Model.Event exposing (InputEvent)


preventDefaultOn : Editor msg -> InternalEditorMsg -> ( InternalEditorMsg, Bool )
preventDefaultOn editor msg =
    case msg of
        BeforeInputEvent inputEvent ->
            ( msg, shouldPreventDefault editor inputEvent )

        _ ->
            ( msg, False )


shouldPreventDefault : Editor msg -> InputEvent -> Bool
shouldPreventDefault editor inputEvent =
    case handleInputEvent editor inputEvent of
        Err _ ->
            False

        Ok _ ->
            True


preventDefaultOnBeforeInputDecoder : Editor msg -> D.Decoder ( msg, Bool )
preventDefaultOnBeforeInputDecoder editor =
    D.map (\( i, b ) -> ( decoder editor i, b )) (D.map (preventDefaultOn editor) beforeInputDecoder)


beforeInputDecoder : D.Decoder InternalEditorMsg
beforeInputDecoder =
    D.map BeforeInputEvent
        (D.map3 InputEvent
            (D.maybe (D.field "data" D.string))
            (D.oneOf [ D.field "isComposing" D.bool, D.succeed False ])
            (D.field "inputType" D.string)
        )


handleInputEvent : Editor msg -> InputEvent -> Result String (Editor msg)
handleInputEvent editor inputEvent =
    let
        namedCommandList =
            namedCommandListFromInputEvent inputEvent (commandMap editor)
    in
    applyNamedCommandList namedCommandList editor


handleBeforeInput : InputEvent -> Editor msg -> Editor msg
handleBeforeInput inputEvent editor =
    case handleInputEvent editor inputEvent of
        Err _ ->
            editor

        Ok newEditor ->
            -- HACK: Android has very strange behavior with regards to before input events, e.g.
            -- prevent default doesn't actually stop the DOM from being modified, so
            -- we're forcing a rerender if we update the editor state on a command
            forceRerender newEditor
