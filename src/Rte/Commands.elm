module Rte.Commands exposing (..)

import Array
import Array.Extra
import Dict exposing (Dict)
import List.Extra
import Regex
import Rte.DeleteWord as DeleteWord
import Rte.Marks exposing (ToggleAction(..), clearMarks, findMarksFromInlineLeaf, hasMarkWithName, toggleMark)
import Rte.Model exposing (ChildNodes(..), Command, CommandBinding(..), CommandMap, Editor, EditorBlockNode, EditorInlineLeaf(..), EditorState, ElementParameters, Mark, NodePath, Selection)
import Rte.Node exposing (EditorFragment(..), EditorNode(..), allRange, concatMap, findBackwardFromExclusive, findForwardFrom, findForwardFromExclusive, findTextBlockNodeAncestor, indexedFoldl, indexedMap, isSelectable, map, next, nodeAt, previous, removeInRange, removeNodeAndEmptyParents, replace, replaceWithFragment, splitBlockAtPathAndOffset, splitTextLeaf)
import Rte.NodePath as NodePath exposing (commonAncestor, decrement, increment, parent, toString)
import Rte.Selection exposing (caretSelection, clearSelectionMarks, isCollapsed, markSelection, normalizeSelection, rangeSelection, selectionFromMarks, singleNodeRangeSelection)


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


deleteKey : String
deleteKey =
    "Delete"


set : List CommandBinding -> Command -> CommandMap -> CommandMap
set bindings func map =
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


stack : List CommandBinding -> Command -> CommandMap -> CommandMap
stack bindings func map =
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


otherwiseDo : Command -> Command -> Command
otherwiseDo b a =
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


backspaceCommands =
    removeRangeSelection
        |> otherwiseDo removeSelectedLeafElementCommand
        |> otherwiseDo backspaceInlineElement
        |> otherwiseDo backspaceBlockNode
        |> otherwiseDo joinBackward


deleteCommands =
    removeRangeSelection
        |> otherwiseDo removeSelectedLeafElementCommand
        |> otherwiseDo deleteInlineElement
        |> otherwiseDo deleteBlockNode
        |> otherwiseDo joinForward


defaultCommandBindings =
    emptyCommandBinding
        |> set [ inputEvent "insertLineBreak", key [ shiftKey, enterKey ], key [ shiftKey, enterKey ] ] insertLineBreak
        |> set [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ] (liftEmpty |> otherwiseDo splitBlock)
        |> set [ inputEvent "deleteContentBackward", key [ backspaceKey ] ]
            (backspaceCommands
                |> otherwiseDo backspaceText
            )
        |> set [ inputEvent "deleteWordBackward", key [ altKey, backspaceKey ] ] (backspaceCommands |> otherwiseDo backspaceWord)
        |> set [ inputEvent "deleteContentForward", key [ deleteKey ] ]
            (deleteCommands
                |> otherwiseDo deleteText
            )
        |> set [ inputEvent "deleteWordForward", key [ altKey, deleteKey ] ] (deleteCommands |> otherwiseDo deleteWord)
        |> set [ key [ metaKey, "a" ] ] selectAll


joinBackward : Command
joinBackward editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsBeginningOfTextBlock selection editorState.root then
                Err "I cannot join a selection that is not the beginning of a text block"

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


joinForward : Command
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
                                        case replace p1 (BlockNodeWrapper newBlock) removed of
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
    findTextBlock findForwardFromExclusive


findPreviousTextBlock : NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findPreviousTextBlock =
    findTextBlock findBackwardFromExclusive


removeRangeSelection : Command
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
                                            removeInRange
                                                (increment normalizedSelection.anchorNode)
                                                (decrement normalizedSelection.focusNode)
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


insertLineBreak : Command
insertLineBreak =
    insertInlineElement (InlineLeaf { name = "br", attributes = [], marks = [] })


insertInlineElement : EditorInlineLeaf -> Command
insertInlineElement leaf editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (insertInlineElement leaf)

            else
                case nodeAt selection.anchorNode editorState.root of
                    Nothing ->
                        Err "Invalid selection"

                    Just node ->
                        case node of
                            InlineLeafWrapper il ->
                                case il of
                                    InlineLeaf _ ->
                                        case replace selection.anchorNode (InlineLeafWrapper leaf) editorState.root of
                                            Err e ->
                                                Err e

                                            Ok newRoot ->
                                                let
                                                    newSelection =
                                                        case
                                                            findForwardFrom
                                                                (\_ n -> isSelectable n)
                                                                selection.anchorNode
                                                                newRoot
                                                        of
                                                            Nothing ->
                                                                Nothing

                                                            Just ( p, _ ) ->
                                                                Just (caretSelection p 0)
                                                in
                                                Ok { editorState | root = newRoot, selection = newSelection }

                                    TextLeaf tl ->
                                        let
                                            ( before, after ) =
                                                splitTextLeaf selection.anchorOffset tl
                                        in
                                        case
                                            replaceWithFragment
                                                selection.anchorNode
                                                (InlineLeafFragment (Array.fromList [ TextLeaf before, leaf, TextLeaf after ]))
                                                editorState.root
                                        of
                                            Err e ->
                                                Err e

                                            Ok newRoot ->
                                                let
                                                    newSelection =
                                                        case
                                                            findForwardFromExclusive
                                                                (\_ n -> isSelectable n)
                                                                selection.anchorNode
                                                                newRoot
                                                        of
                                                            Nothing ->
                                                                Nothing

                                                            Just ( p, _ ) ->
                                                                Just (caretSelection p 0)
                                                in
                                                Ok { editorState | root = newRoot, selection = newSelection }

                            _ ->
                                Err "I can not insert an inline element in a block node"


splitBlock : Command
splitBlock editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen splitBlock

            else
                case findTextBlockNodeAncestor selection.anchorNode editorState.root of
                    Nothing ->
                        Err "I can only split nodes that have a text block ancestor"

                    Just ( textBlockPath, textBlockNode ) ->
                        let
                            relativePath =
                                List.drop (List.length textBlockPath) selection.anchorNode
                        in
                        case splitBlockAtPathAndOffset relativePath selection.anchorOffset textBlockNode of
                            Nothing ->
                                Err <| "Can not split block at path " ++ toString selection.anchorNode

                            Just ( before, after ) ->
                                case replaceWithFragment textBlockPath (BlockNodeFragment (Array.fromList [ before, after ])) editorState.root of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelectionPath =
                                                increment textBlockPath ++ [ 0 ]

                                            newSelection =
                                                caretSelection newSelectionPath 0
                                        in
                                        Ok { editorState | root = newRoot, selection = Just newSelection }


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
                        InlineLeaf _ ->
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
                            replace nodePath (InlineLeafWrapper textNode) root

        Nothing ->
            Err <| "There is no node at node path " ++ toString nodePath


removeSelectedLeafElementCommand : Command
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
                        case findBackwardFromExclusive (\_ n -> isSelectable n) selection.anchorNode editorState.root of
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



-- backspace logic for text
-- offset = 0, try to delete the previous text node's text
-- offset = 1, set the text node to empty
-- other offset, allow browser to do the default behavior


backspaceText : Command
backspaceText editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only backspace a collapsed selection"

            else if selection.anchorOffset > 1 then
                {-
                   -- This would be the logic if we didn't revert to using the native behavior.
                   removeRangeSelection
                       { editorState
                           | selection =
                               Just <|
                                   singleNodeRangeSelection
                                       selection.anchorNode
                                       (selection.anchorOffset - 1)
                                       selection.anchorOffset
                       }
                -}
                Err <|
                    "I use native behavior when doing backspace when the "
                        ++ "anchor offset could not result in a node change"

            else
                case nodeAt selection.anchorNode editorState.root of
                    Nothing ->
                        Err "Invalid selection"

                    Just node ->
                        case node of
                            BlockNodeWrapper _ ->
                                Err "I cannot backspace a block node"

                            InlineLeafWrapper il ->
                                case il of
                                    InlineLeaf _ ->
                                        Err "I cannot backspace text of an inline leaf"

                                    TextLeaf tl ->
                                        if selection.anchorOffset == 1 then
                                            case replace selection.anchorNode (InlineLeafWrapper (TextLeaf { tl | text = String.dropLeft 1 tl.text })) editorState.root of
                                                Err s ->
                                                    Err s

                                                Ok newRoot ->
                                                    let
                                                        newSelection =
                                                            caretSelection selection.anchorNode 0
                                                    in
                                                    Ok { editorState | root = newRoot, selection = Just newSelection }

                                        else
                                            case previous selection.anchorNode editorState.root of
                                                Nothing ->
                                                    Err "No previous node to backspace text"

                                                Just ( previousPath, previousNode ) ->
                                                    case previousNode of
                                                        InlineLeafWrapper previousInlineLeafWrapper ->
                                                            case previousInlineLeafWrapper of
                                                                TextLeaf previousTextLeaf ->
                                                                    let
                                                                        l =
                                                                            String.length previousTextLeaf.text

                                                                        newSelection =
                                                                            singleNodeRangeSelection previousPath l (max 0 (l - 1))
                                                                    in
                                                                    removeRangeSelection { editorState | selection = Just newSelection }

                                                                InlineLeaf _ ->
                                                                    Err "Cannot backspace the text of an inline leaf"

                                                        BlockNodeWrapper _ ->
                                                            Err "Cannot backspace the text of a block node"


isBlockOrInlineNodeWithMark : String -> EditorNode -> Bool
isBlockOrInlineNodeWithMark markName node =
    case node of
        InlineLeafWrapper il ->
            hasMarkWithName markName (findMarksFromInlineLeaf il)

        _ ->
            True


toggleMarkSingleInlineNode : Mark -> ToggleAction -> EditorState -> Result String EditorState
toggleMarkSingleInlineNode mark action editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if selection.anchorNode /= selection.focusNode then
                Err "I can only toggle a single inline node"

            else
                let
                    normalizedSelection =
                        normalizeSelection selection
                in
                case nodeAt normalizedSelection.anchorNode editorState.root of
                    Nothing ->
                        Err "No node at selection"

                    Just node ->
                        case node of
                            BlockNodeWrapper _ ->
                                Err "Cannot toggle a block node"

                            InlineLeafWrapper il ->
                                let
                                    newMarks =
                                        toggleMark action mark (findMarksFromInlineLeaf il)

                                    leaves =
                                        case il of
                                            InlineLeaf leaf ->
                                                [ InlineLeaf { leaf | marks = newMarks } ]

                                            TextLeaf leaf ->
                                                if String.length leaf.text == normalizedSelection.focusOffset && normalizedSelection.anchorOffset == 0 then
                                                    [ TextLeaf { leaf | marks = newMarks } ]

                                                else
                                                    let
                                                        newNode =
                                                            TextLeaf
                                                                { leaf
                                                                    | marks = newMarks
                                                                    , text =
                                                                        String.slice
                                                                            normalizedSelection.anchorOffset
                                                                            normalizedSelection.focusOffset
                                                                            leaf.text
                                                                }

                                                        left =
                                                            TextLeaf { leaf | text = String.left normalizedSelection.anchorOffset leaf.text }

                                                        right =
                                                            TextLeaf { leaf | text = String.dropLeft normalizedSelection.focusOffset leaf.text }
                                                    in
                                                    if normalizedSelection.anchorOffset == 0 then
                                                        [ newNode, right ]

                                                    else if String.length leaf.text == normalizedSelection.focusOffset then
                                                        [ left, newNode ]

                                                    else
                                                        [ left, newNode, right ]

                                    path =
                                        if normalizedSelection.anchorOffset == 0 then
                                            normalizedSelection.anchorNode

                                        else
                                            increment normalizedSelection.anchorNode

                                    newSelection =
                                        singleNodeRangeSelection
                                            path
                                            0
                                            (normalizedSelection.focusOffset - normalizedSelection.anchorOffset)
                                in
                                case
                                    replaceWithFragment
                                        normalizedSelection.anchorNode
                                        (InlineLeafFragment <| Array.fromList leaves)
                                        editorState.root
                                of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        Ok { editorState | selection = Just newSelection, root = newRoot }


toggleMarkOnInlineNodes : Mark -> Command
toggleMarkOnInlineNodes mark editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if selection.focusNode == selection.anchorNode then
                toggleMarkSingleInlineNode mark Flip editorState

            else
                let
                    normalizedSelection =
                        normalizeSelection selection

                    toggleAction =
                        if
                            allRange
                                (isBlockOrInlineNodeWithMark mark.name)
                                normalizedSelection.anchorNode
                                normalizedSelection.focusNode
                                editorState.root
                        then
                            Remove

                        else
                            Add

                    betweenRoot =
                        case next normalizedSelection.anchorNode editorState.root of
                            Nothing ->
                                editorState.root

                            Just ( afterAnchor, _ ) ->
                                case previous normalizedSelection.focusNode editorState.root of
                                    Nothing ->
                                        editorState.root

                                    Just ( beforeFocus, _ ) ->
                                        case
                                            indexedMap
                                                (\path node ->
                                                    if path < afterAnchor || path > beforeFocus then
                                                        node

                                                    else
                                                        case node of
                                                            BlockNodeWrapper _ ->
                                                                node

                                                            InlineLeafWrapper il ->
                                                                case il of
                                                                    InlineLeaf leaf ->
                                                                        InlineLeafWrapper <| InlineLeaf { leaf | marks = toggleMark toggleAction mark leaf.marks }

                                                                    TextLeaf leaf ->
                                                                        InlineLeafWrapper <| TextLeaf { leaf | marks = toggleMark toggleAction mark leaf.marks }
                                                )
                                                (BlockNodeWrapper editorState.root)
                                        of
                                            BlockNodeWrapper bn ->
                                                bn

                                            _ ->
                                                editorState.root

                    modifiedEndNodeEditorState =
                        Result.withDefault { editorState | root = betweenRoot } <|
                            toggleMarkSingleInlineNode
                                mark
                                toggleAction
                                { root = betweenRoot
                                , selection = Just (singleNodeRangeSelection normalizedSelection.focusNode 0 normalizedSelection.focusOffset)
                                }

                    modifiedStartNodeEditorState =
                        case nodeAt normalizedSelection.anchorNode editorState.root of
                            Nothing ->
                                modifiedEndNodeEditorState

                            Just node ->
                                case node of
                                    InlineLeafWrapper il ->
                                        let
                                            focusOffset =
                                                case il of
                                                    TextLeaf leaf ->
                                                        String.length leaf.text

                                                    InlineLeaf _ ->
                                                        0
                                        in
                                        Result.withDefault modifiedEndNodeEditorState <|
                                            toggleMarkSingleInlineNode
                                                mark
                                                toggleAction
                                                { modifiedEndNodeEditorState
                                                    | selection = Just (singleNodeRangeSelection normalizedSelection.anchorNode normalizedSelection.anchorOffset focusOffset)
                                                }

                                    _ ->
                                        modifiedEndNodeEditorState

                    incrementAnchorOffset =
                        normalizedSelection.anchorOffset /= 0

                    anchorAndFocusHaveSameParent =
                        parent normalizedSelection.anchorNode == parent normalizedSelection.focusNode

                    newSelection =
                        rangeSelection
                            (if incrementAnchorOffset then
                                increment normalizedSelection.anchorNode

                             else
                                normalizedSelection.anchorNode
                            )
                            0
                            (if incrementAnchorOffset && anchorAndFocusHaveSameParent then
                                increment normalizedSelection.focusNode

                             else
                                normalizedSelection.focusNode
                            )
                            normalizedSelection.focusOffset
                in
                Ok { modifiedStartNodeEditorState | selection = Just newSelection }


findClosestBlockPath : NodePath -> EditorBlockNode -> NodePath
findClosestBlockPath path node =
    case nodeAt path node of
        Nothing ->
            []

        Just n ->
            case n of
                BlockNodeWrapper _ ->
                    path

                InlineLeafWrapper _ ->
                    parent path


toggleBlock : List String -> String -> String -> Command
toggleBlock allowedBlocks onTag offTag editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected."

        Just selection ->
            let
                normalizedSelection =
                    normalizeSelection selection

                anchorPath =
                    findClosestBlockPath normalizedSelection.anchorNode editorState.root

                focusPath =
                    findClosestBlockPath normalizedSelection.focusNode editorState.root

                doOffBehavior =
                    allRange
                        (\node ->
                            case node of
                                BlockNodeWrapper bn ->
                                    bn.parameters.name == onTag

                                _ ->
                                    True
                        )
                        anchorPath
                        focusPath
                        editorState.root

                newTag =
                    if doOffBehavior then
                        offTag

                    else
                        onTag

                newRoot =
                    case
                        indexedMap
                            (\path node ->
                                if path < anchorPath || path > focusPath then
                                    node

                                else
                                    case node of
                                        BlockNodeWrapper bn ->
                                            let
                                                p =
                                                    bn.parameters
                                            in
                                            if List.member p.name allowedBlocks then
                                                BlockNodeWrapper { bn | parameters = { p | name = newTag } }

                                            else
                                                node

                                        InlineLeafWrapper _ ->
                                            node
                            )
                            (BlockNodeWrapper editorState.root)
                    of
                        BlockNodeWrapper bn ->
                            bn

                        _ ->
                            editorState.root
            in
            Ok { editorState | root = newRoot }


wrap : ElementParameters -> Command
wrap elementParameters editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            let
                normalizedSelection =
                    normalizeSelection selection

                markedRoot =
                    markSelection normalizedSelection editorState.root

                anchorBlock =
                    findClosestBlockPath normalizedSelection.anchorNode markedRoot

                focusBlock =
                    findClosestBlockPath normalizedSelection.focusNode markedRoot

                ancestor =
                    commonAncestor anchorBlock focusBlock
            in
            if ancestor == anchorBlock || ancestor == focusBlock then
                case nodeAt ancestor markedRoot of
                    Nothing ->
                        Err "I cannot find a node at selection"

                    Just node ->
                        let
                            newChildren =
                                case node of
                                    BlockNodeWrapper bn ->
                                        BlockArray (Array.fromList [ bn ])

                                    InlineLeafWrapper il ->
                                        InlineLeafArray (Array.fromList [ il ])

                            newNode =
                                { parameters = elementParameters, childNodes = newChildren }
                        in
                        case replace ancestor (BlockNodeWrapper newNode) markedRoot of
                            Err err ->
                                Err err

                            Ok newRoot ->
                                Ok
                                    { editorState
                                        | root = clearSelectionMarks newRoot
                                        , selection =
                                            selectionFromMarks
                                                newRoot
                                                selection.anchorOffset
                                                selection.focusOffset
                                    }

            else
                case List.Extra.getAt (List.length ancestor) normalizedSelection.anchorNode of
                    Nothing ->
                        Err "Invalid ancestor path at anchor node"

                    Just childAnchorIndex ->
                        case List.Extra.getAt (List.length ancestor) normalizedSelection.focusNode of
                            Nothing ->
                                Err "Invalid ancestor path at focus node"

                            Just childFocusIndex ->
                                case nodeAt ancestor markedRoot of
                                    Nothing ->
                                        Err "Invalid common ancestor path"

                                    Just node ->
                                        case node of
                                            BlockNodeWrapper bn ->
                                                case bn.childNodes of
                                                    BlockArray a ->
                                                        let
                                                            newChildNode =
                                                                { parameters = elementParameters
                                                                , childNodes = BlockArray (Array.slice childAnchorIndex (childFocusIndex + 1) a)
                                                                }

                                                            newBlockArray =
                                                                BlockArray
                                                                    (Array.append
                                                                        (Array.append
                                                                            (Array.Extra.sliceUntil childAnchorIndex a)
                                                                            (Array.fromList [ newChildNode ])
                                                                        )
                                                                        (Array.Extra.sliceFrom (childFocusIndex + 1) a)
                                                                    )

                                                            newNode =
                                                                { bn | childNodes = newBlockArray }
                                                        in
                                                        case replace ancestor (BlockNodeWrapper newNode) markedRoot of
                                                            Err s ->
                                                                Err s

                                                            Ok newRoot ->
                                                                Ok
                                                                    { editorState
                                                                        | root = clearSelectionMarks newRoot
                                                                        , selection =
                                                                            selectionFromMarks
                                                                                newRoot
                                                                                selection.anchorOffset
                                                                                selection.focusOffset
                                                                    }

                                                    InlineLeafArray _ ->
                                                        Err "Cannot wrap inline elements"

                                                    Leaf ->
                                                        Err "Cannot wrap leaf elements"

                                            InlineLeafWrapper _ ->
                                                Err "Invalid ancestor path... somehow we have an inline leaf"


selectAll : Command
selectAll editorState =
    let
        ( fl, lastOffset ) =
            indexedFoldl
                (\path node ( firstAndLast, offset ) ->
                    if isSelectable node then
                        let
                            newOffset =
                                case node of
                                    InlineLeafWrapper il ->
                                        case il of
                                            TextLeaf tl ->
                                                String.length tl.text

                                            InlineLeaf _ ->
                                                0

                                    BlockNodeWrapper _ ->
                                        0
                        in
                        case firstAndLast of
                            Nothing ->
                                ( Just ( path, path ), newOffset )

                            Just ( first, _ ) ->
                                ( Just ( first, path ), newOffset )

                    else
                        ( firstAndLast, offset )
                )
                ( Nothing, 0 )
                (BlockNodeWrapper editorState.root)
    in
    case fl of
        Nothing ->
            Err "Nothing is selectable"

        Just ( first, last ) ->
            Ok { editorState | selection = Just <| rangeSelection first 0 last lastOffset }



-- mark each text block to lift
-- for each block, lift it out of its container if possible


liftMark =
    { name = "__lift__", attributes = [] }


addLiftMarkToBlocksInSelection : Selection -> EditorBlockNode -> EditorBlockNode
addLiftMarkToBlocksInSelection selection root =
    let
        start =
            findClosestBlockPath selection.anchorNode root

        end =
            findClosestBlockPath selection.focusNode root
    in
    case
        indexedMap
            (\path node ->
                if path < start || path > end then
                    node

                else
                    case node of
                        BlockNodeWrapper bn ->
                            let
                                parameters =
                                    bn.parameters

                                addMarker =
                                    case bn.childNodes of
                                        Leaf ->
                                            True

                                        InlineLeafArray _ ->
                                            True

                                        _ ->
                                            False
                            in
                            if addMarker then
                                BlockNodeWrapper
                                    { bn
                                        | parameters =
                                            { parameters
                                                | marks =
                                                    toggleMark Add liftMark bn.parameters.marks
                                            }
                                    }

                            else
                                node

                        _ ->
                            node
            )
            (BlockNodeWrapper root)
    of
        BlockNodeWrapper bn ->
            bn

        _ ->
            root


liftConcatMapFunc : EditorNode -> List EditorNode
liftConcatMapFunc node =
    case node of
        BlockNodeWrapper bn ->
            case bn.childNodes of
                Leaf ->
                    [ node ]

                InlineLeafArray _ ->
                    [ node ]

                BlockArray a ->
                    let
                        groupedBlockNodes =
                            List.Extra.groupWhile
                                (\n1 n2 ->
                                    hasMarkWithName liftMark.name n1.parameters.marks == hasMarkWithName liftMark.name n2.parameters.marks
                                )
                                (Array.toList a)
                    in
                    List.map BlockNodeWrapper <|
                        List.concatMap
                            (\( n, l ) ->
                                if hasMarkWithName liftMark.name n.parameters.marks then
                                    n :: l

                                else
                                    [ { bn | childNodes = BlockArray (Array.fromList <| n :: l) } ]
                            )
                            groupedBlockNodes

        InlineLeafWrapper _ ->
            [ node ]


lift : Command
lift editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            let
                normalizedSelection =
                    normalizeSelection selection

                markedRoot =
                    addLiftMarkToBlocksInSelection normalizedSelection <| markSelection normalizedSelection editorState.root

                liftedRoot =
                    concatMap liftConcatMapFunc markedRoot

                newSelection =
                    selectionFromMarks liftedRoot normalizedSelection.anchorOffset normalizedSelection.focusOffset
            in
            Ok
                { editorState
                    | selection = newSelection
                    , root = clearMarks liftMark <| clearSelectionMarks liftedRoot
                }


liftEmpty : Command
liftEmpty editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if (not <| isCollapsed selection) || selection.anchorOffset /= 0 then
                Err "Can only lift empty text blocks"

            else
                let
                    p =
                        findClosestBlockPath selection.anchorNode editorState.root
                in
                case nodeAt p editorState.root of
                    Nothing ->
                        Err "Invalid root path"

                    Just node ->
                        if not <| isEmptyTextBlock node then
                            Err "I cannot lift a node that is not an empty text block"

                        else if List.length p < 2 then
                            Err "I cannot lift a node that's root or an immediate child of root"

                        else
                            lift editorState


isEmptyTextBlock : EditorNode -> Bool
isEmptyTextBlock node =
    case node of
        BlockNodeWrapper bn ->
            case bn.childNodes of
                InlineLeafArray a ->
                    case Array.get 0 a of
                        Nothing ->
                            Array.isEmpty a

                        Just n ->
                            Array.length a
                                == 1
                                && (case n of
                                        TextLeaf t ->
                                            String.isEmpty t.text

                                        _ ->
                                            False
                                   )

                _ ->
                    False

        InlineLeafWrapper _ ->
            False


splitBlockHeaderToNewParagraph : List String -> String -> Command
splitBlockHeaderToNewParagraph headerElements paragraphElement editorState =
    case splitBlock editorState of
        Err s ->
            Err s

        Ok splitEditorState ->
            case splitEditorState.selection of
                Nothing ->
                    Ok splitEditorState

                Just selection ->
                    if (not <| isCollapsed selection) || selection.anchorOffset /= 0 then
                        Ok splitEditorState

                    else
                        let
                            p =
                                findClosestBlockPath selection.anchorNode splitEditorState.root
                        in
                        case nodeAt p splitEditorState.root of
                            Nothing ->
                                Ok splitEditorState

                            Just node ->
                                case node of
                                    BlockNodeWrapper bn ->
                                        let
                                            parameters =
                                                bn.parameters
                                        in
                                        if List.member parameters.name headerElements && isEmptyTextBlock node then
                                            case
                                                replace p
                                                    (BlockNodeWrapper
                                                        { bn
                                                            | parameters = { parameters | name = paragraphElement }
                                                        }
                                                    )
                                                    splitEditorState.root
                                            of
                                                Err _ ->
                                                    Ok splitEditorState

                                                Ok newRoot ->
                                                    Ok { splitEditorState | root = newRoot }

                                        else
                                            Ok splitEditorState

                                    _ ->
                                        Ok splitEditorState


insertBlockNode : EditorBlockNode -> Command
insertBlockNode node editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (insertBlockNode node)

            else
                case nodeAt selection.anchorNode editorState.root of
                    Nothing ->
                        Err "Invalid selection"

                    Just anchorNode ->
                        case anchorNode of
                            -- if a block node is selected, then insert after the selected block
                            BlockNodeWrapper bn ->
                                case replaceWithFragment selection.anchorNode (BlockNodeFragment (Array.fromList [ bn, node ])) editorState.root of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelection =
                                                if isSelectable (BlockNodeWrapper node) then
                                                    caretSelection (increment selection.anchorNode) 0

                                                else
                                                    selection
                                        in
                                        Ok { editorState | selection = Just newSelection, root = newRoot }

                            -- if an inline node is selected, then split the block and insert before
                            InlineLeafWrapper _ ->
                                case splitBlock editorState of
                                    Err s ->
                                        Err s

                                    Ok splitEditorState ->
                                        insertBlockNodeBeforeSelection node splitEditorState


insertBlockNodeBeforeSelection : EditorBlockNode -> Command
insertBlockNodeBeforeSelection node editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only insert a block element before a collapsed selection"

            else
                let
                    markedRoot =
                        markSelection selection editorState.root

                    closestBlockPath =
                        findClosestBlockPath selection.anchorNode markedRoot
                in
                case nodeAt closestBlockPath markedRoot of
                    Nothing ->
                        Err "Invalid selection"

                    Just anchorNode ->
                        case anchorNode of
                            BlockNodeWrapper bn ->
                                let
                                    newFragment =
                                        if isEmptyTextBlock <| BlockNodeWrapper bn then
                                            [ node ]

                                        else
                                            [ node, bn ]
                                in
                                case replaceWithFragment closestBlockPath (BlockNodeFragment (Array.fromList newFragment)) markedRoot of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelection =
                                                if isSelectable (BlockNodeWrapper node) then
                                                    Just <| caretSelection closestBlockPath 0

                                                else
                                                    selectionFromMarks newRoot selection.anchorOffset selection.focusOffset
                                        in
                                        Ok { editorState | selection = newSelection, root = clearSelectionMarks newRoot }

                            -- if an inline node is selected, then split the block and insert before
                            InlineLeafWrapper _ ->
                                Err "Invalid state! I was expecting a block node."


backspaceInlineElement : Command
backspaceInlineElement editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only backspace an inline element if the selection is collapsed"

            else if selection.anchorOffset /= 0 then
                Err "I can only backspace an inline element if the offset is 0"

            else
                let
                    decrementedPath =
                        decrement selection.anchorNode
                in
                case nodeAt decrementedPath editorState.root of
                    Nothing ->
                        Err "There is no previous inline element"

                    Just node ->
                        case node of
                            InlineLeafWrapper il ->
                                case il of
                                    InlineLeaf _ ->
                                        case replaceWithFragment decrementedPath (InlineLeafFragment Array.empty) editorState.root of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                Ok
                                                    { editorState
                                                        | selection =
                                                            Just <| caretSelection decrementedPath 0
                                                        , root = newRoot
                                                    }

                                    TextLeaf _ ->
                                        Err "There is no previous inline leaf element, found a text leaf"

                            BlockNodeWrapper _ ->
                                Err "There is no previous inline leaf element, found a block node"


backspaceBlockNode : Command
backspaceBlockNode editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsBeginningOfTextBlock selection editorState.root then
                Err "Cannot backspace a block element if we're not at the beginning of a text block"

            else
                let
                    blockPath =
                        findClosestBlockPath selection.anchorNode editorState.root

                    markedRoot =
                        markSelection selection editorState.root
                in
                case previous blockPath editorState.root of
                    Nothing ->
                        Err "There is no previous element to backspace"

                    Just ( path, node ) ->
                        case node of
                            BlockNodeWrapper bn ->
                                case bn.childNodes of
                                    Leaf ->
                                        case replaceWithFragment path (BlockNodeFragment Array.empty) markedRoot of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                Ok
                                                    { editorState
                                                        | root = clearSelectionMarks newRoot
                                                        , selection = selectionFromMarks newRoot selection.anchorOffset selection.focusOffset
                                                    }

                                    _ ->
                                        Err "The previous element is not a block leaf"

                            InlineLeafWrapper _ ->
                                Err "The previous element is not a block node"


groupSameTypeInlineLeaf : EditorInlineLeaf -> EditorInlineLeaf -> Bool
groupSameTypeInlineLeaf a b =
    case a of
        InlineLeaf _ ->
            case b of
                InlineLeaf _ ->
                    True

                TextLeaf _ ->
                    False

        TextLeaf _ ->
            case b of
                TextLeaf _ ->
                    True

                InlineLeaf _ ->
                    False


textFromGroup : List EditorInlineLeaf -> String
textFromGroup leaves =
    String.join "" <|
        List.map
            (\leaf ->
                case leaf of
                    TextLeaf t ->
                        t.text

                    _ ->
                        ""
            )
            leaves


lengthsFromGroup : List EditorInlineLeaf -> List Int
lengthsFromGroup leaves =
    List.map
        (\il ->
            case il of
                TextLeaf tl ->
                    String.length tl.text

                InlineLeaf _ ->
                    0
        )
        leaves



-- Find the inline fragment that represents connected text nodes
-- get the text in that fragment
-- translate the offset for that text
-- find where to backspace


backspaceWord : Command
backspaceWord editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot remove a word of a range selection"

            else
                case findTextBlockNodeAncestor selection.anchorNode editorState.root of
                    Nothing ->
                        Err "I can only remove a word on a text leaf"

                    Just ( p, n ) ->
                        case n.childNodes of
                            InlineLeafArray arr ->
                                let
                                    groupedLeaves =
                                        -- group text nodes together
                                        List.Extra.groupWhile
                                            groupSameTypeInlineLeaf
                                            (Array.toList arr)
                                in
                                case List.Extra.last selection.anchorNode of
                                    Nothing ->
                                        Err "Somehow the anchor node is the root node"

                                    Just lastIndex ->
                                        let
                                            ( relativeLastIndex, group ) =
                                                List.foldl
                                                    (\( first, rest ) ( i, g ) ->
                                                        if not <| List.isEmpty g then
                                                            ( i, g )

                                                        else if List.length rest + 1 > i then
                                                            ( i, first :: rest )

                                                        else
                                                            ( i - (List.length rest + 1), g )
                                                    )
                                                    ( lastIndex, [] )
                                                    groupedLeaves

                                            groupText =
                                                textFromGroup group

                                            offsetUpToNewIndex =
                                                List.sum <|
                                                    List.take
                                                        relativeLastIndex
                                                    <|
                                                        lengthsFromGroup group

                                            offset =
                                                offsetUpToNewIndex + selection.anchorOffset

                                            stringFrom =
                                                String.left offset groupText
                                        in
                                        if String.isEmpty stringFrom then
                                            Err "Cannot remove word a word if the text fragment is empty"

                                        else
                                            let
                                                matches =
                                                    Regex.findAtMost 1 DeleteWord.backspaceWordRegex stringFrom

                                                matchOffset =
                                                    case List.head matches of
                                                        Nothing ->
                                                            0

                                                        Just match ->
                                                            match.index

                                                ( newGroupIndex, newOffset, _ ) =
                                                    List.foldl
                                                        (\l ( i, o, done ) ->
                                                            if done then
                                                                ( i, o, done )

                                                            else if l < o then
                                                                ( i + 1, o - l, False )

                                                            else
                                                                ( i, o, True )
                                                        )
                                                        ( 0, matchOffset, False )
                                                    <|
                                                        lengthsFromGroup group

                                                newIndex =
                                                    lastIndex - (relativeLastIndex - newGroupIndex)

                                                newSelection =
                                                    rangeSelection (p ++ [ newIndex ]) newOffset selection.anchorNode selection.anchorOffset

                                                newState =
                                                    { editorState | selection = Just newSelection }
                                            in
                                            removeRangeSelection newState

                            _ ->
                                Err "I expected an inline leaf array"


deleteText : Command
deleteText editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only backspace a collapsed selection"

            else
                case nodeAt selection.anchorNode editorState.root of
                    Nothing ->
                        Err "I was given an invalid path to delete text"

                    Just node ->
                        case node of
                            BlockNodeWrapper _ ->
                                Err "I cannot delete text if the selection a block node"

                            InlineLeafWrapper il ->
                                case il of
                                    InlineLeaf _ ->
                                        Err "I cannot delete text if the selection an inline leaf"

                                    TextLeaf tl ->
                                        let
                                            textLength =
                                                String.length tl.text
                                        in
                                        if selection.anchorOffset < (textLength - 1) then
                                            Err "I use the default behavior when deleting text when the anchor offset is not at the end of a text node"

                                        else if selection.anchorOffset == (textLength - 1) then
                                            case replace selection.anchorNode (InlineLeafWrapper (TextLeaf { tl | text = String.dropRight 1 tl.text })) editorState.root of
                                                Err s ->
                                                    Err s

                                                Ok newRoot ->
                                                    Ok { editorState | root = newRoot }

                                        else
                                            case next selection.anchorNode editorState.root of
                                                Nothing ->
                                                    Err "I cannot do delete because there is no neighboring text node"

                                                Just ( nextPath, nextNode ) ->
                                                    case nextNode of
                                                        BlockNodeWrapper _ ->
                                                            Err "Cannot delete the text of a block node"

                                                        InlineLeafWrapper nextInlineLeafWrapper ->
                                                            case nextInlineLeafWrapper of
                                                                TextLeaf _ ->
                                                                    let
                                                                        newSelection =
                                                                            singleNodeRangeSelection nextPath 0 1
                                                                    in
                                                                    removeRangeSelection { editorState | selection = Just newSelection }

                                                                InlineLeaf _ ->
                                                                    Err "Cannot backspace the text of an inline leaf"


deleteInlineElement : Command
deleteInlineElement editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only delete an inline element if the selection is collapsed"

            else
                case nodeAt selection.anchorNode editorState.root of
                    Nothing ->
                        Err "I was given an invalid path to delete text"

                    Just node ->
                        case node of
                            BlockNodeWrapper _ ->
                                Err "I cannot delete text if the selection a block node"

                            InlineLeafWrapper il ->
                                let
                                    length =
                                        case il of
                                            TextLeaf t ->
                                                String.length t.text

                                            InlineLeaf _ ->
                                                0
                                in
                                if length < selection.anchorOffset then
                                    Err "I cannot delete an inline element if the cursor is not at the end of an inline node"

                                else
                                    let
                                        incrementedPath =
                                            increment selection.anchorNode
                                    in
                                    case nodeAt incrementedPath editorState.root of
                                        Nothing ->
                                            Err "There is no next inline leaf to delete"

                                        Just incrementedNode ->
                                            case incrementedNode of
                                                InlineLeafWrapper nil ->
                                                    case nil of
                                                        InlineLeaf _ ->
                                                            case replaceWithFragment incrementedPath (InlineLeafFragment Array.empty) editorState.root of
                                                                Err s ->
                                                                    Err s

                                                                Ok newRoot ->
                                                                    Ok { editorState | root = newRoot }

                                                        TextLeaf _ ->
                                                            Err "There is no next inline leaf element, found a text leaf"

                                                BlockNodeWrapper _ ->
                                                    Err "There is no next inline leaf, found a block node"


deleteBlockNode : Command
deleteBlockNode editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsEndOfTextBlock selection editorState.root then
                Err "Cannot delete a block element if we're not at the end of a text block"

            else
                case next selection.anchorNode editorState.root of
                    Nothing ->
                        Err "There is no next node to delete"

                    Just ( path, node ) ->
                        case node of
                            BlockNodeWrapper bn ->
                                case bn.childNodes of
                                    Leaf ->
                                        case replaceWithFragment path (BlockNodeFragment Array.empty) editorState.root of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                Ok { editorState | root = clearSelectionMarks newRoot }

                                    _ ->
                                        Err "The next node is not a block leaf"

                            InlineLeafWrapper _ ->
                                Err "The next node is not a block leaf, it's an inline leaf"


deleteWord : Command
deleteWord editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot remove a word of a range selection"

            else
                case findTextBlockNodeAncestor selection.anchorNode editorState.root of
                    Nothing ->
                        Err "I can only remove a word on a text leaf"

                    Just ( p, n ) ->
                        case n.childNodes of
                            InlineLeafArray arr ->
                                let
                                    groupedLeaves =
                                        List.Extra.groupWhile
                                            groupSameTypeInlineLeaf
                                            (Array.toList arr)
                                in
                                case List.Extra.last selection.anchorNode of
                                    Nothing ->
                                        Err "Somehow the anchor node is the root node"

                                    Just lastIndex ->
                                        let
                                            ( relativeLastIndex, group ) =
                                                List.foldl
                                                    (\( first, rest ) ( i, g ) ->
                                                        if not <| List.isEmpty g then
                                                            ( i, g )

                                                        else if List.length rest + 1 > i then
                                                            ( i, first :: rest )

                                                        else
                                                            ( i - (List.length rest + 1), g )
                                                    )
                                                    ( lastIndex, [] )
                                                    groupedLeaves

                                            groupText =
                                                textFromGroup group

                                            offsetUpToNewIndex =
                                                List.sum <|
                                                    List.take
                                                        relativeLastIndex
                                                    <|
                                                        lengthsFromGroup group

                                            offset =
                                                offsetUpToNewIndex + selection.anchorOffset

                                            stringTo =
                                                String.dropLeft offset groupText
                                        in
                                        if String.isEmpty stringTo then
                                            Err "Cannot remove word a word if the text fragment is empty"

                                        else
                                            let
                                                matches =
                                                    Regex.findAtMost 1 DeleteWord.deleteWordRegex stringTo

                                                matchOffset =
                                                    case List.head matches of
                                                        Nothing ->
                                                            0

                                                        Just match ->
                                                            match.index + String.length match.match

                                                ( newGroupIndex, newOffset, _ ) =
                                                    List.foldl
                                                        (\l ( i, o, done ) ->
                                                            if done then
                                                                ( i, o, done )

                                                            else if l < o then
                                                                ( i + 1, o - l, False )

                                                            else
                                                                ( i, o, True )
                                                        )
                                                        ( 0, offset + matchOffset, False )
                                                    <|
                                                        lengthsFromGroup group

                                                newIndex =
                                                    lastIndex - (relativeLastIndex - newGroupIndex)

                                                newSelection =
                                                    rangeSelection (p ++ [ newIndex ]) newOffset selection.anchorNode selection.anchorOffset

                                                newState =
                                                    { editorState | selection = Just newSelection }
                                            in
                                            removeRangeSelection newState

                            _ ->
                                Err "I expected an inline leaf array"
