module RichTextEditor.Internal.BeforeInput exposing (..)

import Json.Decode as D
import RichTextEditor.Internal.Editor exposing (applyNamedCommandList)
import RichTextEditor.Model.Command exposing (CommandMap, namedCommandListFromInputEvent)
import RichTextEditor.Model.Editor
    exposing
        ( Editor
        , InternalEditorMsg(..)
        , Tagger
        , forceRerender
        )
import RichTextEditor.Model.Event exposing (InputEvent)
import RichTextEditor.Model.Spec exposing (Spec)


preventDefaultOn : CommandMap -> Spec -> Editor -> InternalEditorMsg -> ( InternalEditorMsg, Bool )
preventDefaultOn commandMap spec editor msg =
    case msg of
        BeforeInputEvent inputEvent ->
            ( msg, shouldPreventDefault commandMap spec editor inputEvent )

        _ ->
            ( msg, False )


shouldPreventDefault : CommandMap -> Spec -> Editor -> InputEvent -> Bool
shouldPreventDefault commandMap spec editor inputEvent =
    case handleInputEvent commandMap spec editor inputEvent of
        Err _ ->
            False

        Ok _ ->
            True


preventDefaultOnBeforeInputDecoder : Tagger msg -> CommandMap -> Spec -> Editor -> D.Decoder ( msg, Bool )
preventDefaultOnBeforeInputDecoder tagger commandMap spec editor =
    D.map (\( i, b ) -> ( tagger i, b )) (D.map (preventDefaultOn commandMap spec editor) beforeInputDecoder)


beforeInputDecoder : D.Decoder InternalEditorMsg
beforeInputDecoder =
    D.map BeforeInputEvent
        (D.map3 InputEvent
            (D.maybe (D.field "data" D.string))
            (D.oneOf [ D.field "isComposing" D.bool, D.succeed False ])
            (D.field "inputType" D.string)
        )


handleInputEvent : CommandMap -> Spec -> Editor -> InputEvent -> Result String Editor
handleInputEvent commandMap spec editor inputEvent =
    let
        namedCommandList =
            namedCommandListFromInputEvent inputEvent commandMap
    in
    applyNamedCommandList namedCommandList spec editor


handleBeforeInput : InputEvent -> CommandMap -> Spec -> Editor -> Editor
handleBeforeInput inputEvent commandMap spec editor =
    case handleInputEvent commandMap spec editor inputEvent of
        Err _ ->
            editor

        Ok newEditor ->
            -- HACK: Android has very strange behavior with regards to before input events, e.g.
            -- prevent default doesn't actually stop the DOM from being modified, so
            -- we're forcing a rerender if we update the editor state on a command
            forceRerender newEditor
