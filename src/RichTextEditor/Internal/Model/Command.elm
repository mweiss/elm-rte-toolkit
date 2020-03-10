module RichTextEditor.Internal.Model.Command exposing
    ( Command
    , CommandBinding
    , CommandMap
    , InternalAction(..)
    , NamedCommand
    , NamedCommandList
    , Transform
    , inputEvent
    , internalCommand
    , key
    , namedCommandListFromInputEvent
    , transformCommand
    )

import Dict exposing (Dict)
import RichTextEditor.Internal.Model.EditorState exposing (State)
import RichTextEditor.Internal.Model.Event exposing (InputEvent, KeyboardEvent)


type alias Transform =
    State -> Result String State


type InternalAction
    = Undo
    | Redo


type Command
    = TransformCommand Transform
    | InternalCommand InternalAction


transformCommand : Transform -> Command
transformCommand t =
    TransformCommand t


internalCommand : InternalAction -> Command
internalCommand i =
    InternalCommand i


type alias NamedCommand =
    ( String, Command )


type alias NamedCommandList =
    List NamedCommand


{-| A key map is a dictionary of sorted keys to a command function. Keys should be created with the
Command.key function to ensure the list is sorted in the correct order. This is used to determine
what command to execute on the editor's keydown event.
-}
type alias KeyMap =
    Dict (List String) NamedCommandList


{-| A dictionary of input type event type to command function. This is used to determine what command
to execute on the editor's beforeinput events.
-}
type alias InputEventTypeMap =
    Dict String NamedCommandList


type CommandMap
    = CommandMap CommandMapContents


namedCommandListFromInputEvent : InputEvent -> CommandMap -> NamedCommandList
namedCommandListFromInputEvent event map =
    case map of
        CommandMap contents ->
            Maybe.withDefault (contents.defaultInputEventCommand event)
                (Dict.get event.inputType contents.inputEventTypeMap)


{-| A command map holds a map of key combination and input events to actions that should be taken.
You can use this map to create custom commands on key press.
-}
type alias CommandMapContents =
    { keyMap : KeyMap
    , inputEventTypeMap : InputEventTypeMap
    , defaultKeyCommand : KeyboardEvent -> NamedCommandList
    , defaultInputEventCommand : InputEvent -> NamedCommandList
    }


{-| A command binding can either be a key press or an input event. Note that each browser has
varying degrees of Input Level 2 support, so relying just on input events is usually not enough to support
all browsers. On the flip side, virtual keyboards, specifically Android virtual keyboards, can fire
synthetic keyboard events that don't contain the key value, so for key actions on those platforms
you may need rely on input events.
-}
type CommandBinding
    = Key (List String)
    | InputEventType String


inputEvent type_ =
    InputEventType type_


key keys =
    Key <| List.sort keys
