module Rte.Commands exposing (..)

import Dict exposing (Dict)
import Rte.Model exposing (CommandBinding(..), CommandFunc, CommandMap, Editor, EditorState)


altKey : String
altKey =
    "Alt"


metaKey : String
metaKey =
    "Meta"


ctrlKey : String
ctrlKey =
    "Ctrl"


shiftKey : String
shiftKey =
    "Shift"


returnKey : String
returnKey =
    "Return"


enterKey : String
enterKey =
    "Enter"


backspaceKey : String
backspaceKey =
    "Backspace"


setCommand : List CommandBinding -> CommandFunc -> CommandMap -> CommandMap
setCommand bindings func map =
    List.foldl
        (\binding accMap ->
            case binding of
                Key keys ->
                    { accMap | keyMap = Dict.insert keys func accMap.keyMap }

                InputEventType type_ ->
                    { accMap | inputEventTypeMap = Dict.insert type_ func accMap.inputEventTypeMap }
        )
        map
        bindings


stackCommands : List CommandBinding -> CommandFunc -> CommandMap -> CommandMap
stackCommands bindings func map =
    List.foldl
        (\binding accMap ->
            case binding of
                Key keys ->
                    case Dict.get keys accMap.keyMap of
                        Nothing ->
                            { accMap | keyMap = Dict.insert keys func accMap.keyMap }

                        Just f ->
                            { accMap | keyMap = Dict.insert keys (otherwiseDo func f) accMap.keyMap }

                InputEventType type_ ->
                    case Dict.get type_ accMap.inputEventTypeMap of
                        Nothing ->
                            { accMap | inputEventTypeMap = Dict.insert type_ func accMap.inputEventTypeMap }

                        Just f ->
                            { accMap | inputEventTypeMap = Dict.insert type_ (otherwiseDo func f) accMap.inputEventTypeMap }
        )
        map
        bindings


otherwiseDo : CommandFunc -> CommandFunc -> CommandFunc
otherwiseDo a b =
    \s ->
        case a s of
            Nothing ->
                b s

            Just v ->
                Just v


emptyCommandBinding =
    { keyMap = Dict.empty, inputEventTypeMap = Dict.empty }


inputEvent type_ =
    InputEventType type_


key keys =
    Key <| List.sort keys


defaultCommandBindings =
    emptyCommandBinding
        |> setCommand [ inputEvent "insertLineBreak", key [ shiftKey, enterKey ], key [ shiftKey, enterKey ] ] insertLineBreakCommand
        |> setCommand [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ] splitBlockCommand
        |> setCommand [ inputEvent "deleteContentBackward", key [ backspaceKey ] ] (backspaceBlockElementCommand |> otherwiseDo backspaceInlineElementCommand |> otherwiseDo backspaceCommand)


insertLineBreakCommand : CommandFunc
insertLineBreakCommand editorState =
    Nothing


splitBlockCommand : CommandFunc
splitBlockCommand editorState =
    Nothing


headerToNewParagraphCommand : List String -> String -> CommandFunc
headerToNewParagraphCommand headerElements paragraphElement editorState =
    Nothing


backspaceBlockElementCommand : CommandFunc
backspaceBlockElementCommand editorState =
    Nothing


backspaceInlineElementCommand : CommandFunc
backspaceInlineElementCommand editorState =
    Nothing


backspaceCommand : CommandFunc
backspaceCommand editorState =
    Nothing


backspaceWord : CommandFunc
backspaceWord editorState =
    Nothing


delete : CommandFunc
delete editorState =
    Nothing


deleteWord : CommandFunc
deleteWord editorState =
    Nothing
