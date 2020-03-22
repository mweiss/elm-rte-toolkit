module RichTextEditor.Model.Command exposing
    ( Command(..)
    , CommandBinding
    , CommandMap
    , InternalAction(..)
    , NamedCommand
    , NamedCommandList
    , Transform
    , combine
    , emptyCommandMap
    , inputEvent
    , internalCommand
    , key
    , namedCommandListFromInputEvent
    , namedCommandListFromKeyboardEvent
    , set
    , stack
    , transformCommand
    , withDefaultInputEventCommand
    , withDefaultKeyCommand
    )

import Dict exposing (Dict)
import RichTextEditor.Model.Event exposing (InputEvent, KeyboardEvent)
import RichTextEditor.Model.Keys exposing (alt, ctrl, meta, shift, short)
import RichTextEditor.Model.State exposing (State)


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
        shift :: keys

    else
        keys


addMetaKey : KeyboardEvent -> List String -> List String
addMetaKey keyboardEvent keys =
    if keyboardEvent.metaKey then
        meta :: keys

    else
        keys


addCtrlKey : KeyboardEvent -> List String -> List String
addCtrlKey keyboardEvent keys =
    if keyboardEvent.ctrlKey then
        ctrl :: keys

    else
        keys


addAltKey : KeyboardEvent -> List String -> List String
addAltKey keyboardEvent keys =
    if keyboardEvent.altKey then
        alt :: keys

    else
        keys


namedCommandListFromKeyboardEvent : String -> KeyboardEvent -> CommandMap -> NamedCommandList
namedCommandListFromKeyboardEvent shortKey event map =
    case map of
        CommandMap contents ->
            let
                mapping =
                    keyboardEventToDictKey event

                shortKeyReplaced =
                    List.map
                        (\v ->
                            if v == shortKey then
                                short

                            else
                                v
                        )
                        mapping
            in
            case Dict.get shortKeyReplaced contents.keyMap of
                Nothing ->
                    case Dict.get mapping contents.keyMap of
                        Nothing ->
                            contents.defaultKeyCommand event

                        Just v ->
                            v

                Just v ->
                    case Dict.get mapping contents.keyMap of
                        Nothing ->
                            v

                        Just v2 ->
                            v ++ v2


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


set : List CommandBinding -> NamedCommandList -> CommandMap -> CommandMap
set bindings func map =
    List.foldl
        (\binding accMap ->
            case accMap of
                CommandMap m ->
                    case binding of
                        Key keys ->
                            CommandMap { m | keyMap = Dict.insert keys func m.keyMap }

                        InputEventType type_ ->
                            CommandMap { m | inputEventTypeMap = Dict.insert type_ func m.inputEventTypeMap }
        )
        map
        bindings


withDefaultKeyCommand : (KeyboardEvent -> NamedCommandList) -> CommandMap -> CommandMap
withDefaultKeyCommand func map =
    case map of
        CommandMap m ->
            CommandMap { m | defaultKeyCommand = func }


withDefaultInputEventCommand : (InputEvent -> NamedCommandList) -> CommandMap -> CommandMap
withDefaultInputEventCommand func map =
    case map of
        CommandMap m ->
            CommandMap { m | defaultInputEventCommand = func }


compose : comparable -> NamedCommandList -> Dict comparable NamedCommandList -> Dict comparable NamedCommandList
compose k commandList d =
    case Dict.get k d of
        Nothing ->
            Dict.insert k commandList d

        Just v ->
            Dict.insert k (commandList ++ v) d


combine : CommandMap -> CommandMap -> CommandMap
combine m1 m2 =
    case m1 of
        CommandMap map1 ->
            case m2 of
                CommandMap map2 ->
                    CommandMap
                        { inputEventTypeMap =
                            Dict.foldl
                                compose
                                map2.inputEventTypeMap
                                map1.inputEventTypeMap
                        , keyMap = Dict.foldl compose map2.keyMap map1.keyMap
                        , defaultKeyCommand =
                            if map1.defaultKeyCommand == emptyFunction then
                                map2.defaultKeyCommand

                            else
                                map1.defaultKeyCommand
                        , defaultInputEventCommand =
                            if map1.defaultInputEventCommand == emptyFunction then
                                map2.defaultInputEventCommand

                            else
                                map1.defaultInputEventCommand
                        }


stack : List CommandBinding -> NamedCommandList -> CommandMap -> CommandMap
stack bindings list map =
    List.foldl
        (\binding accMap ->
            case accMap of
                CommandMap m ->
                    CommandMap <|
                        case binding of
                            Key keys ->
                                case Dict.get keys m.keyMap of
                                    Nothing ->
                                        { m | keyMap = Dict.insert keys list m.keyMap }

                                    Just f ->
                                        { m | keyMap = Dict.insert keys (list ++ f) m.keyMap }

                            InputEventType type_ ->
                                case Dict.get type_ m.inputEventTypeMap of
                                    Nothing ->
                                        { m | inputEventTypeMap = Dict.insert type_ list m.inputEventTypeMap }

                                    Just f ->
                                        { m | inputEventTypeMap = Dict.insert type_ (list ++ f) m.inputEventTypeMap }
        )
        map
        bindings


emptyFunction =
    \_ -> []


emptyCommandMap =
    CommandMap
        { keyMap = Dict.empty
        , inputEventTypeMap = Dict.empty
        , defaultKeyCommand = emptyFunction
        , defaultInputEventCommand = emptyFunction
        }
