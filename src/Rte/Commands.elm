module Rte.Commands exposing (..)

import Array
import Dict exposing (Dict)
import List.Extra
import Rte.Marks as ToggleAction exposing (ToggleAction, findMarksFromInlineLeaf, hasMarkWithName, toggleMark)
import Rte.Model exposing (ChildNodes(..), CommandBinding(..), CommandFunc, CommandMap, Editor, EditorBlockNode, EditorInlineLeaf(..), EditorState, ElementParameters, Mark, NodePath, Selection)
import Rte.Node exposing (EditorFragment(..), EditorNode(..), allRange, findBackwardFromExclusive, findForwardFrom, findForwardFromExclusive, findTextBlockNodeAncestor, indexedMap, isSelectable, next, nodeAt, previous, removeInRange, removeNodeAndEmptyParents, replace, replaceWithFragment, splitBlockAtPathAndOffset, splitTextLeaf)
import Rte.NodePath as NodePath exposing (decrementNodePath, incrementNodePath, parentPath, toString)
import Rte.Selection exposing (caretSelection, isCollapsed, normalizeSelection, rangeSelection, singleNodeRangeSelection)


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


set : List CommandBinding -> CommandFunc -> CommandMap -> CommandMap
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


stack : List CommandBinding -> CommandFunc -> CommandMap -> CommandMap
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
        |> set [ inputEvent "insertLineBreak", key [ shiftKey, enterKey ], key [ shiftKey, enterKey ] ] insertLineBreak
        |> set [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ] splitBlock
        |> set [ inputEvent "deleteContentBackward", key [ backspaceKey ] ] (removeRangeSelection |> otherwiseDo removeSelectedLeafElementCommand |> otherwiseDo backspaceInlineElement |> otherwiseDo joinBackward |> otherwiseDo backspaceText)


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
                                            removeInRange
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


insertLineBreak : CommandFunc
insertLineBreak =
    insertInlineElement (InlineLeaf { name = "br", attributes = [], marks = [] })


insertInlineElement : EditorInlineLeaf -> CommandFunc
insertInlineElement leaf editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen splitBlock

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


splitBlock : CommandFunc
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
                                                incrementNodePath textBlockPath ++ [ 0 ]

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


backspaceText : CommandFunc
backspaceText editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only backspace a collapsed selection"

            else if selection.anchorOffset > 1 then
                Err "When the offset is greater than 1, we'll allow the default browser behavior to occur"

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


backspaceWord : CommandFunc
backspaceWord editorState =
    Err "Not implemented"


delete : CommandFunc
delete editorState =
    Err "Not implemented"


deleteWord : CommandFunc
deleteWord editorState =
    Err "Not implemented"


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
                                            incrementNodePath normalizedSelection.anchorNode

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


toggleMarkOnInlineNodes : Mark -> CommandFunc
toggleMarkOnInlineNodes mark editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if selection.focusNode == selection.anchorNode then
                toggleMarkSingleInlineNode mark ToggleAction.Flip editorState

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
                            ToggleAction.Remove

                        else
                            ToggleAction.Add

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
                        parentPath normalizedSelection.anchorNode == parentPath normalizedSelection.focusNode

                    newSelection =
                        rangeSelection
                            (if incrementAnchorOffset then
                                incrementNodePath normalizedSelection.anchorNode

                             else
                                normalizedSelection.anchorNode
                            )
                            0
                            (if incrementAnchorOffset && anchorAndFocusHaveSameParent then
                                incrementNodePath normalizedSelection.focusNode

                             else
                                normalizedSelection.focusNode
                            )
                            normalizedSelection.focusOffset
                in
                Ok { modifiedStartNodeEditorState | selection = Just newSelection }


wrapIn : ElementParameters -> CommandFunc
wrapIn elementParameters editorState =
    Err "Not implemented"


selectAll : CommandFunc
selectAll editorState =
    Err "Not implemented"


lift : CommandFunc
lift editorState =
    Err "Not implemented"


headerToNewParagraph : List String -> String -> CommandFunc
headerToNewParagraph headerElements paragraphElement editorState =
    Err "Not implemented"


backspaceInlineElement : CommandFunc
backspaceInlineElement editorState =
    Err "Not implemented"


backspaceBlockElement : CommandFunc
backspaceBlockElement editorState =
    Err "Not implemented"
