module RichText.Internal.BeforeInput exposing (..)

import Json.Decode as D
import RichText.Config.Command exposing (CommandMap, namedCommandListFromInputEvent)
import RichText.Config.Spec exposing (Spec)
import RichText.Internal.Editor
    exposing
        ( Editor
        , Message(..)
        , Tagger
        , applyNamedCommandList
        , forceRerender
        )
import RichText.Internal.Event exposing (InputEvent)


preventDefaultOn : CommandMap -> Spec -> Editor -> Message -> ( Message, Bool )
preventDefaultOn commandMap spec editor msg =
    case msg of
        BeforeInputEvent inputEvent ->
            if inputEvent.isComposing then
                ( msg, False )

            else
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


beforeInputDecoder : D.Decoder Message
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
    if inputEvent.isComposing then
        editor

    else
        case handleInputEvent commandMap spec editor inputEvent of
            Err _ ->
                editor

            Ok newEditor ->
                -- HACK: Android has very strange behavior with regards to before input events, e.g.
                -- prevent default doesn't actually stop the DOM from being modified, so
                -- we're forcing a rerender if we update the editor state on a command
                forceRerender newEditor
