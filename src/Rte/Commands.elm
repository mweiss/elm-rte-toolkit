module Rte.Commands exposing (..)

import Array
import Dict exposing (Dict)
import List.Extra
import Rte.Model exposing (ChildNodes(..), CommandBinding(..), CommandFunc, CommandMap, Editor, EditorBlockNode, EditorInlineLeaf(..), EditorState, NodePath, Selection)
import Rte.Node exposing (EditorNode(..), findNodeBackwardFromExclusive, findNodeForwardFromExclusive, findTextBlockNodeAncestor, isSelectable, nodeAt, removeNodeAndEmptyParents, removeNodesInRange, replaceNode)
import Rte.NodePath as NodePath exposing (decrementNodePath, incrementNodePath, toString)
import Rte.Selection exposing (caretSelection, isCollapsed, normalizeSelection)


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
        |> setCommand [ inputEvent "deleteContentBackward", key [ backspaceKey ] ] (removeRangeSelection |> otherwiseDo removeSelectedLeafElementCommand |> otherwiseDo backspaceInlineElementCommand |> otherwiseDo joinBackward |> otherwiseDo backspaceCommand)


joinBackward : CommandFunc
joinBackward editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsBeginningOfTextBlock selection editorState.root then
                Err "I cannot join a range selection"

            else
                case findTextBlockNodeAncestor selection.anchorNode editorState.root of
                    Nothing ->
                        Err "There is no text block at the selection"

                    Just ( textBlockPath, _ ) ->
                        case findPreviousTextBlock textBlockPath editorState.root of
                            Nothing ->
                                Err "There is no text block I can join backward with"

                            Just ( p, n ) ->
                                -- We're going to transpose this into joinForward by setting the selection to the end of the
                                -- given text block
                                case n.childNodes of
                                    InlineLeafArray a ->
                                        case Array.get (Array.length a - 1) a of
                                            Nothing ->
                                                Err "There must be at least one element in the inline node to join with"

                                            Just leaf ->
                                                let
                                                    newSelection =
                                                        case leaf of
                                                            TextLeaf tl ->
                                                                caretSelection (p ++ [ Array.length a - 1 ]) (String.length tl.text)

                                                            InlineLeaf _ ->
                                                                caretSelection (p ++ [ Array.length a - 1 ]) 0
                                                in
                                                joinForward { editorState | selection = Just newSelection }

                                    _ ->
                                        Err "I can only join with text blocks"


selectionIsBeginningOfTextBlock : Selection -> EditorBlockNode -> Bool
selectionIsBeginningOfTextBlock selection root =
    if not <| isCollapsed selection then
        False

    else
        case findTextBlockNodeAncestor selection.anchorNode root of
            Nothing ->
                False

            Just ( _, n ) ->
                case n.childNodes of
                    InlineLeafArray a ->
                        case List.Extra.last selection.anchorNode of
                            Nothing ->
                                False

                            Just i ->
                                if i /= 0 || Array.isEmpty a then
                                    False

                                else
                                    selection.anchorOffset == 0

                    _ ->
                        False


selectionIsEndOfTextBlock : Selection -> EditorBlockNode -> Bool
selectionIsEndOfTextBlock selection root =
    if not <| isCollapsed selection then
        False

    else
        case findTextBlockNodeAncestor selection.anchorNode root of
            Nothing ->
                False

            Just ( _, n ) ->
                case n.childNodes of
                    InlineLeafArray a ->
                        case List.Extra.last selection.anchorNode of
                            Nothing ->
                                False

                            Just i ->
                                if i /= Array.length a - 1 then
                                    False

                                else
                                    case Array.get i a of
                                        Nothing ->
                                            False

                                        Just leaf ->
                                            case leaf of
                                                TextLeaf tl ->
                                                    String.length tl.text == selection.anchorOffset

                                                InlineLeaf _ ->
                                                    True

                    _ ->
                        False


joinForward : CommandFunc
joinForward editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsEndOfTextBlock selection editorState.root then
                Err "I cannot join a selection that is not at the end of a text block"

            else
                case findTextBlockNodeAncestor selection.anchorNode editorState.root of
                    Nothing ->
                        Err "The selection has no text block ancestor"

                    Just ( p1, n1 ) ->
                        case findNextTextBlock selection.anchorNode editorState.root of
                            Nothing ->
                                Err "There is no text block I can join forward with"

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


isTextBlock : NodePath -> EditorNode -> Bool
isTextBlock _ node =
    case node of
        BlockNodeWrapper bn ->
            case bn.childNodes of
                InlineLeafArray _ ->
                    True

                _ ->
                    False

        _ ->
            False


type alias FindFunc =
    (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )


findTextBlock : FindFunc -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findTextBlock findFunc path node =
    case
        findFunc
            isTextBlock
            path
            node
    of
        Nothing ->
            Nothing

        Just ( p, n ) ->
            case n of
                BlockNodeWrapper bn ->
                    Just ( p, bn )

                _ ->
                    Nothing


findNextTextBlock : NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findNextTextBlock =
    findTextBlock findNodeForwardFromExclusive


findPreviousTextBlock : NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findPreviousTextBlock =
    findTextBlock findNodeBackwardFromExclusive


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


isLeafNode : NodePath -> EditorBlockNode -> Bool
isLeafNode path root =
    case nodeAt path root of
        Nothing ->
            False

        Just node ->
            case node of
                BlockNodeWrapper bn ->
                    if bn.childNodes == Leaf then
                        True

                    else
                        False

                InlineLeafWrapper l ->
                    case l of
                        InlineLeaf _ ->
                            True

                        TextLeaf _ ->
                            False


removeTextAtRange : NodePath -> Int -> Maybe Int -> EditorBlockNode -> Result String EditorBlockNode
removeTextAtRange nodePath start maybeEnd root =
    case nodeAt nodePath root of
        Just node ->
            case node of
                BlockNodeWrapper _ ->
                    Err "I was expecting a text node, but instead I got a block node"

                InlineLeafWrapper leaf ->
                    case leaf of
                        InlineLeaf l ->
                            Err "I was expecting a text leaf, but instead I got an inline leaf"

                        TextLeaf v ->
                            let
                                textNode =
                                    case maybeEnd of
                                        Nothing ->
                                            TextLeaf { v | text = String.left start v.text }

                                        Just end ->
                                            TextLeaf { v | text = String.left start v.text ++ String.dropLeft end v.text }
                            in
                            replaceNode nodePath (InlineLeafWrapper textNode) root

        Nothing ->
            Err <| "There is no node at node path " ++ toString nodePath


removeSelectedLeafElementCommand : CommandFunc
removeSelectedLeafElementCommand editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot remove a block element if it is not"

            else if isLeafNode selection.anchorNode editorState.root then
                let
                    newSelection =
                        case findNodeBackwardFromExclusive (\_ n -> isSelectable n) selection.anchorNode editorState.root of
                            Nothing ->
                                Nothing

                            Just ( p, n ) ->
                                let
                                    offset =
                                        case n of
                                            InlineLeafWrapper il ->
                                                case il of
                                                    TextLeaf t ->
                                                        String.length t.text

                                                    _ ->
                                                        0

                                            _ ->
                                                0
                                in
                                Just (caretSelection p offset)
                in
                Ok
                    { editorState
                        | root = removeNodeAndEmptyParents selection.anchorNode editorState.root
                        , selection = newSelection
                    }

            else
                Err "There's no leaf node at the given selection"


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
