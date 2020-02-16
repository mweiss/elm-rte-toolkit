module Rte.Commands exposing (..)

import Array
import Dict exposing (Dict)
import Rte.CommandUtils exposing (removeTextAtRange)
import Rte.Model exposing (ChildNodes(..), CommandBinding(..), CommandFunc, CommandMap, Editor, EditorBlockNode, EditorState, NodePath)
import Rte.NodePath as NodePath exposing (decrementNodePath, incrementNodePath)
import Rte.NodeUtils exposing (EditorNode(..), findNodeForwardFromExclusive, findTextBlockNodeAncestor, removeNodeAndEmptyParents, removeNodesInRange, replaceNode)
import Rte.Selection exposing (caretSelection, isCollapsed, markSelection, normalizeSelection)


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
            Err _ ->
                b s

            Ok v ->
                Ok v


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
        |> setCommand [ inputEvent "deleteContentBackward", key [ backspaceKey ] ] (removeRangeSelection |> otherwiseDo backspaceBlockElementCommand |> otherwiseDo backspaceInlineElementCommand |> otherwiseDo backspaceCommand)


joinForward : CommandFunc
joinForward editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot join a range selection"

            else
                case findTextBlockNodeAncestor selection.anchorNode editorState.root of
                    Nothing ->
                        Err "The selection has no text block ancestor"

                    Just ( p1, n1 ) ->
                        case findNextTextBlock selection.anchorNode editorState.root of
                            Nothing ->
                                Err "There is no text block I can join with"

                            Just ( p2, n2 ) ->
                                case joinBlocks n1 n2 of
                                    Nothing ->
                                        Err <|
                                            "I could not join these two blocks at"
                                                ++ NodePath.toString p1
                                                ++ " ,"
                                                ++ NodePath.toString p2

                                    Just newBlock ->
                                        let
                                            removed =
                                                removeNodeAndEmptyParents p2 editorState.root
                                        in
                                        case replaceNode p1 (BlockNodeWrapper newBlock) removed of
                                            Err e ->
                                                Err e

                                            Ok b ->
                                                Ok { editorState | root = b }


joinBlocks : EditorBlockNode -> EditorBlockNode -> Maybe EditorBlockNode
joinBlocks b1 b2 =
    case b1.childNodes of
        BlockArray a1 ->
            case b2.childNodes of
                BlockArray a2 ->
                    Just { b1 | childNodes = BlockArray (Array.append a1 a2) }

                _ ->
                    Nothing

        InlineLeafArray a1 ->
            case b2.childNodes of
                InlineLeafArray a2 ->
                    Just { b1 | childNodes = InlineLeafArray (Array.append a1 a2) }

                _ ->
                    Nothing

        Leaf ->
            Nothing


findNextTextBlock : NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findNextTextBlock nodePath blockNode =
    case
        findNodeForwardFromExclusive
            (\_ n ->
                case n of
                    BlockNodeWrapper bn ->
                        case bn.childNodes of
                            InlineLeafArray _ ->
                                True

                            _ ->
                                False

                    _ ->
                        False
            )
            nodePath
            blockNode
    of
        Nothing ->
            Nothing

        Just ( p, n ) ->
            case n of
                BlockNodeWrapper bn ->
                    Just ( p, bn )

                _ ->
                    Nothing


removeRangeSelection : CommandFunc
removeRangeSelection editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if isCollapsed selection then
                Err "Cannot remove contents of collapsed selection"

            else
                let
                    normalizedSelection =
                        normalizeSelection selection
                in
                if normalizedSelection.anchorNode == normalizedSelection.focusNode then
                    case removeTextAtRange normalizedSelection.anchorNode normalizedSelection.anchorOffset (Just normalizedSelection.focusOffset) editorState.root of
                        Ok newRoot ->
                            let
                                newSelection =
                                    caretSelection normalizedSelection.anchorNode normalizedSelection.anchorOffset
                            in
                            Ok { editorState | root = newRoot, selection = Just newSelection }

                        Err s ->
                            Err s

                else
                    let
                        anchorTextBlock =
                            findTextBlockNodeAncestor normalizedSelection.anchorNode editorState.root

                        focusTextBlock =
                            findTextBlockNodeAncestor normalizedSelection.focusNode editorState.root
                    in
                    case removeTextAtRange normalizedSelection.focusNode 0 (Just normalizedSelection.focusOffset) editorState.root of
                        Err s ->
                            Err s

                        Ok removedEnd ->
                            case removeTextAtRange normalizedSelection.anchorNode normalizedSelection.anchorOffset Nothing removedEnd of
                                Err s ->
                                    Err s

                                Ok removedStart ->
                                    let
                                        removedNodes =
                                            removeNodesInRange
                                                (incrementNodePath normalizedSelection.anchorNode)
                                                (decrementNodePath normalizedSelection.focusNode)
                                                removedStart

                                        newSelection =
                                            caretSelection normalizedSelection.anchorNode normalizedSelection.anchorOffset

                                        newEditorState =
                                            { editorState | root = removedNodes, selection = Just newSelection }
                                    in
                                    if anchorTextBlock == Nothing || anchorTextBlock == focusTextBlock then
                                        Ok newEditorState

                                    else
                                        Ok <| Result.withDefault newEditorState (joinForward newEditorState)


insertLineBreakCommand : CommandFunc
insertLineBreakCommand editorState =
    Err "Not implemented"


splitBlockCommand : CommandFunc
splitBlockCommand editorState =
    Err "Not implemented"


headerToNewParagraphCommand : List String -> String -> CommandFunc
headerToNewParagraphCommand headerElements paragraphElement editorState =
    Err "Not implemented"


backspaceBlockElementCommand : CommandFunc
backspaceBlockElementCommand editorState =
    Err "Not implemented"


backspaceInlineElementCommand : CommandFunc
backspaceInlineElementCommand editorState =
    Err "Not implemented"


backspaceCommand : CommandFunc
backspaceCommand editorState =
    Err "Not implemented"


backspaceWord : CommandFunc
backspaceWord editorState =
    Err "Not implemented"


delete : CommandFunc
delete editorState =
    Err "Not implemented"


deleteWord : CommandFunc
deleteWord editorState =
    Err "Not implemented"
