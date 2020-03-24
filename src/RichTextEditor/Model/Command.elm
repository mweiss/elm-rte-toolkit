module RichTextEditor.Model.Command exposing
    ( Command(..), transform, internal, InternalAction(..), NamedCommand, NamedCommandList
    , CommandMap, CommandBinding, key, inputEvent, emptyCommandMap, set, withDefaultInputEventCommand, withDefaultKeyCommand, defaultKeyCommand, defaultInputEventCommand, combine
    , Transform
    , namedCommandListFromInputEvent, namedCommandListFromKeyboardEvent, InputEvent, KeyboardEvent
    )

{-| This module contains types relating to defining transforms, commands and command maps


# Command

@docs Command, transform, internal, InternalAction, NamedCommand, NamedCommandList


# Command map

@docs CommandMap, CommandBinding, key, inputEvent, emptyCommandMap, set, withDefaultInputEventCommand, withDefaultKeyCommand, defaultKeyCommand, defaultInputEventCommand, combine


# Transform

@docs Transform


# Event

@docs namedCommandListFromInputEvent, namedCommandListFromKeyboardEvent, InputEvent, KeyboardEvent

-}

import Dict exposing (Dict)
import List.Extra
import RichTextEditor.Model.Keys exposing (alt, ctrl, meta, shift, short)
import RichTextEditor.Model.State exposing (State)


{-| A `Transform` is a function that receives a state and returns a result either with a new modified state,
or a String explaining why the transform couldn't be done. With the exception of internal commands, all commands
are defined by one or more transforms.

    removeSelection : Transform
    removeSelection state =
        Ok (state |> withSelection Nothing)

-}
type alias Transform =
    State -> Result String State


{-| `InternalAction` is a fixed set of actions that require internal editor state
to execute. For now this only includes undo and redo.
-}
type InternalAction
    = Undo
    | Redo


{-| `Command` is either a transform or internal action that can be applied to an editor
via `RichTextEditor.Editor.applyCommand`
-}
type Command
    = TransformCommand Transform
    | InternalCommand InternalAction


{-| Creates a command from a transform.

    transform removeSelection
    --> A command from the removeSelection transform

-}
transform : Transform -> Command
transform t =
    TransformCommand t


{-| Creates a command from an internal action.

    internal Undo
    --> A command that will execute undo

-}
internal : InternalAction -> Command
internal i =
    InternalCommand i


{-| Type alias for a command with a name. Giving a command a name is useful for debugging purposes
when debugging a chain of commands. The command name is also stored in the history, which can help
define more advanced behavior.

    ( "removeSelection", removeSelection )

-}
type alias NamedCommand =
    ( String, Command )


{-| Type alias for a list of named commands.

    [ ( "backspaceWord", backspaceWord ), ( "backspace", backspace ) ]

-}
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


{-| A command map provides bindings between keydown/beforeinput events and a list of named commands.

    emptyCommandMap
        |> set
            [ inputEvent "insertLineBreak", key [ shift, enter ], key [ shift, return ] ]
            [ ( "insertLineBreak", transform insertLineBreak ) ]

Note that on a successful command, preventDefault is called on the associated event. This means
that if a key command succeeds, the input command should not be triggered.

-}
type CommandMap
    = CommandMap CommandMapContents


{-| The attributes parsed from an input event.
-}
type alias InputEvent =
    { data : Maybe String, isComposing : Bool, inputType : String }


{-| The attributes parsed from a keyboard event.
-}
type alias KeyboardEvent =
    { keyCode : Int
    , key : String
    , altKey : Bool
    , metaKey : Bool
    , ctrlKey : Bool
    , shiftKey : Bool
    , isComposing : Bool
    }


{-| Derives a named command list from an input event.
-}
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


{-| Derives a named command list from a keyboard event. The first argument is the value of `shortKey`,
a platform dependent shortcut for either Meta or Control.
-}
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
synthetic keyboard events that don't contain the key value, so for some actions on those platforms
you may need rely on input events.
-}
type CommandBinding
    = Key (List String)
    | InputEventType String


{-| Creates an input event key given the input event type.
-}
inputEvent : String -> CommandBinding
inputEvent type_ =
    InputEventType type_


{-| Creates a key binding given a list of keys.
-}
key : List String -> CommandBinding
key keys =
    Key <| List.sort (List.Extra.unique keys)


{-| Returns a `CommandMap` with each command binding set to the given named command list.

    emptyCommandMap
        |> set
            [ inputEvent "insertLineBreak", key [ shift, enter ], key [ shift, return ] ]
            [ ( "insertLineBreak", transform insertLineBreak ) ]

-}
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


{-| `defaultKeyCommand` is used if there are no key bindings set for a keyboard event.
-}
defaultKeyCommand : CommandMap -> (KeyboardEvent -> NamedCommandList)
defaultKeyCommand map =
    case map of
        CommandMap m ->
            m.defaultKeyCommand


{-| `defaultInputEventCommand` is used if there are no input event bindings set for an input event type.
-}
defaultInputEventCommand : CommandMap -> (InputEvent -> NamedCommandList)
defaultInputEventCommand map =
    case map of
        CommandMap m ->
            m.defaultInputEventCommand


{-| Returns a commandMap with the defaultKeyCommand set to the provided value.

    emptyCommandMap
        |> withDefaultKeyCommand func

-}
withDefaultKeyCommand : (KeyboardEvent -> NamedCommandList) -> CommandMap -> CommandMap
withDefaultKeyCommand func map =
    case map of
        CommandMap m ->
            CommandMap { m | defaultKeyCommand = func }


{-| Returns a commandMap with the defaultKeyCommand set to the provided value. The defaultInputEventCommand
is used if there are no key bindings set for a keyboard event.

    emptyCommandMap
        |> withDefaultInputEventCommand func

-}
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


{-| Combines two command maps, with the second `CommandMap` commands being appended to the first
`CommandMap`.

    combine listCommandBindings defaultCommandBindings
    --> Creates a command map that prioritizes list command bindings, then default command bindings

-}
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
                        , defaultKeyCommand = \e -> map1.defaultKeyCommand e ++ map2.defaultKeyCommand e
                        , defaultInputEventCommand = \e -> map1.defaultInputEventCommand e ++ map2.defaultInputEventCommand e
                        }


emptyFunction =
    \_ -> []


{-| An empty command map
-}
emptyCommandMap : CommandMap
emptyCommandMap =
    CommandMap
        { keyMap = Dict.empty
        , inputEventTypeMap = Dict.empty
        , defaultKeyCommand = emptyFunction
        , defaultInputEventCommand = emptyFunction
        }
