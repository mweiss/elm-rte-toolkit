module RichTextEditor.Commands exposing (..)

import Array exposing (Array)
import Array.Extra
import List.Extra
import Regex
import RichTextEditor.Annotation exposing (clearAnnotations)
import RichTextEditor.Internal.DeleteWord as DeleteWord
import RichTextEditor.Marks
    exposing
        ( hasMarkWithName
        , toggleMark
        )
import RichTextEditor.Model.Command
    exposing
        ( CommandBinding
        , CommandMap
        , InternalAction(..)
        , NamedCommandList
        , Transform
        , emptyCommandMap
        , inputEvent
        , internalCommand
        , key
        , set
        , transformCommand
        , withDefaultInputEventCommand
        , withDefaultKeyCommand
        )
import RichTextEditor.Model.Event exposing (InputEvent, KeyboardEvent)
import RichTextEditor.Model.Keys
    exposing
        ( altKey
        , backspaceKey
        , deleteKey
        , enterKey
        , metaKey
        , returnKey
        , shiftKey
        )
import RichTextEditor.Model.Mark as Mark exposing (Mark, MarkOrder, ToggleAction(..), toggle)
import RichTextEditor.Model.Node exposing (BlockArray, BlockNode, ChildNodes(..), ElementParameters, Fragment(..), InlineLeaf(..), Node(..), Path, annotationsFromBlockNode, blockArray, blockNode, blockNodeWithElementParameters, childNodes, comparableElementParameters, elementParameters, elementParametersFromBlockNode, fromBlockArray, fromInlineArray, inlineLeafArray, inlineLeafParameters, inlineLeafParametersWithMarks, marksFromInlineLeaf, nameFromElementParameters, text, textLeafParametersWithMarks, withChildNodes, withText)
import RichTextEditor.Model.Selection
    exposing
        ( Selection
        , anchorNode
        , anchorOffset
        , caretSelection
        , focusNode
        , focusOffset
        , isCollapsed
        , normalize
        , rangeSelection
        , singleNodeRangeSelection
        )
import RichTextEditor.Model.State as State exposing (State, withRoot, withSelection)
import RichTextEditor.Node
    exposing
        ( allRange
        , concatMap
        , findBackwardFromExclusive
        , findClosestBlockPath
        , findForwardFrom
        , findForwardFromExclusive
        , findTextBlockNodeAncestor
        , indexedFoldl
        , indexedMap
        , isSelectable
        , joinBlocks
        , next
        , nodeAt
        , previous
        , removeInRange
        , removeNodeAndEmptyParents
        , replace
        , replaceWithFragment
        , splitBlockAtPathAndOffset
        , splitTextLeaf
        )
import RichTextEditor.NodePath as NodePath
    exposing
        ( commonAncestor
        , decrement
        , increment
        , parent
        , toString
        )
import RichTextEditor.Selection
    exposing
        ( annotateSelection
        , clearSelectionAnnotations
        , selectionFromAnnotations
        )
import RichTextEditor.Specs exposing (hardBreak)
import Set
import String.Extra


backspaceCommands =
    [ ( "removeRangeSelection", transformCommand removeRangeSelection )
    , ( "removeSelectedLeafElementCommand", transformCommand removeSelectedLeafElement )
    , ( "backspaceInlineElement", transformCommand backspaceInlineElement )
    , ( "backspaceBlockNode", transformCommand backspaceBlockNode )
    , ( "joinBackward", transformCommand joinBackward )
    ]


deleteCommands =
    [ ( "removeRangeSelection", transformCommand removeRangeSelection )
    , ( "removeSelectedLeafElementCommand", transformCommand removeSelectedLeafElement )
    , ( "deleteInlineElement", transformCommand deleteInlineElement )
    , ( "deleteBlockNode", transformCommand deleteBlockNode )
    , ( "joinForward", transformCommand joinForward )
    ]


defaultCommandBindings =
    emptyCommandMap
        |> set
            [ inputEvent "insertLineBreak", key [ shiftKey, enterKey ], key [ shiftKey, enterKey ] ]
            [ ( "insertLineBreak", transformCommand insertLineBreak ) ]
        |> set [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ]
            [ ( "liftEmpty", transformCommand liftEmpty ), ( "splitTextBlock", transformCommand splitTextBlock ) ]
        |> set [ inputEvent "deleteContentBackward", key [ backspaceKey ] ]
            (backspaceCommands ++ [ ( "backspaceText", transformCommand backspaceText ) ])
        |> set [ inputEvent "deleteWordBackward", key [ altKey, backspaceKey ] ]
            (backspaceCommands ++ [ ( "backspaceWord", transformCommand backspaceWord ) ])
        |> set [ inputEvent "deleteContentForward", key [ deleteKey ] ]
            (deleteCommands ++ [ ( "deleteText", transformCommand deleteText ) ])
        |> set [ inputEvent "deleteWordForward", key [ altKey, deleteKey ] ]
            (deleteCommands ++ [ ( "deleteWord", transformCommand deleteWord ) ])
        |> set [ key [ metaKey, "a" ] ]
            [ ( "selectAll", transformCommand selectAll ) ]
        |> set [ key [ metaKey, "z" ] ]
            [ ( "undo", internalCommand Undo ) ]
        |> set [ key [ metaKey, shiftKey, "z" ] ]
            [ ( "redo", internalCommand Redo ) ]
        |> withDefaultKeyCommand defaultKeyCommand
        |> withDefaultInputEventCommand defaultInputEventCommand


defaultKeyCommand : KeyboardEvent -> NamedCommandList
defaultKeyCommand event =
    if not event.altKey && not event.metaKey && not event.ctrlKey && String.length event.key == 1 then
        [ ( "removeRangeAndInsert", transformCommand <| removeRangeSelectionAndInsert event.key ) ]

    else
        []


defaultInputEventCommand : InputEvent -> NamedCommandList
defaultInputEventCommand event =
    if event.inputType == "insertText" then
        case event.data of
            Nothing ->
                []

            Just data ->
                [ ( "removeRangeAndInsert", transformCommand <| removeRangeSelectionAndInsert data ) ]

    else
        []


removeRangeSelectionAndInsert : String -> Transform
removeRangeSelectionAndInsert s editorState =
    case removeRangeSelection editorState of
        Err e ->
            Err e

        Ok removedRangeEditorState ->
            Ok <|
                Result.withDefault
                    removedRangeEditorState
                    (insertTextAtSelection s removedRangeEditorState)


insertTextAtSelection : String -> Transform
insertTextAtSelection s editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only insert text if the range is collapsed"

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "Invalid selection after remove range"

                    Just node ->
                        case node of
                            Block _ ->
                                Err "I was expected a text leaf, but instead I found a block node"

                            Inline il ->
                                case il of
                                    InlineLeaf _ ->
                                        Err "I was expecting a text leaf, but instead found a block node"

                                    TextLeaf tl ->
                                        let
                                            newText =
                                                String.Extra.insertAt s (anchorOffset selection) (text tl)

                                            newTextLeaf =
                                                TextLeaf (tl |> withText newText)
                                        in
                                        case
                                            replace
                                                (anchorNode selection)
                                                (Inline newTextLeaf)
                                                (State.root editorState)
                                        of
                                            Err e ->
                                                Err e

                                            Ok newRoot ->
                                                Ok
                                                    (editorState
                                                        |> withRoot newRoot
                                                        |> withSelection
                                                            (Just <|
                                                                caretSelection
                                                                    (anchorNode selection)
                                                                    (anchorOffset selection + 1)
                                                            )
                                                    )


joinBackward : Transform
joinBackward editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsBeginningOfTextBlock selection (State.root editorState) then
                Err "I cannot join a selection that is not the beginning of a text block"

            else
                case findTextBlockNodeAncestor (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "There is no text block at the selection"

                    Just ( textBlockPath, _ ) ->
                        case findPreviousTextBlock textBlockPath (State.root editorState) of
                            Nothing ->
                                Err "There is no text block I can join backward with"

                            Just ( p, n ) ->
                                -- We're going to transpose this into joinForward by setting the selection to the end of the
                                -- given text block
                                case childNodes n of
                                    InlineChildren a ->
                                        let
                                            array =
                                                fromInlineArray a
                                        in
                                        case Array.get (Array.length array - 1) array of
                                            Nothing ->
                                                Err "There must be at least one element in the inline node to join with"

                                            Just leaf ->
                                                let
                                                    newSelection =
                                                        case leaf of
                                                            TextLeaf tl ->
                                                                caretSelection
                                                                    (p ++ [ Array.length array - 1 ])
                                                                    (String.length (text tl))

                                                            InlineLeaf _ ->
                                                                caretSelection
                                                                    (p ++ [ Array.length array - 1 ])
                                                                    0
                                                in
                                                joinForward
                                                    (editorState
                                                        |> withSelection (Just newSelection)
                                                    )

                                    _ ->
                                        Err "I can only join with text blocks"


selectionIsBeginningOfTextBlock : Selection -> BlockNode -> Bool
selectionIsBeginningOfTextBlock selection root =
    if not <| isCollapsed selection then
        False

    else
        case findTextBlockNodeAncestor (anchorNode selection) root of
            Nothing ->
                False

            Just ( _, n ) ->
                case childNodes n of
                    InlineChildren a ->
                        case List.Extra.last (anchorNode selection) of
                            Nothing ->
                                False

                            Just i ->
                                if i /= 0 || Array.isEmpty (fromInlineArray a) then
                                    False

                                else
                                    anchorOffset selection == 0

                    _ ->
                        False


selectionIsEndOfTextBlock : Selection -> BlockNode -> Bool
selectionIsEndOfTextBlock selection root =
    if not <| isCollapsed selection then
        False

    else
        case findTextBlockNodeAncestor (anchorNode selection) root of
            Nothing ->
                False

            Just ( _, n ) ->
                case childNodes n of
                    InlineChildren a ->
                        case List.Extra.last (anchorNode selection) of
                            Nothing ->
                                False

                            Just i ->
                                if i /= Array.length (fromInlineArray a) - 1 then
                                    False

                                else
                                    case Array.get i (fromInlineArray a) of
                                        Nothing ->
                                            False

                                        Just leaf ->
                                            case leaf of
                                                TextLeaf tl ->
                                                    String.length (text tl) == anchorOffset selection

                                                InlineLeaf _ ->
                                                    True

                    _ ->
                        False


joinForward : Transform
joinForward editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsEndOfTextBlock selection (State.root editorState) then
                Err "I cannot join a selection that is not at the end of a text block"

            else
                case findTextBlockNodeAncestor (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "The selection has no text block ancestor"

                    Just ( p1, n1 ) ->
                        case findNextTextBlock (anchorNode selection) (State.root editorState) of
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
                                                removeNodeAndEmptyParents p2 (State.root editorState)
                                        in
                                        case replace p1 (Block newBlock) removed of
                                            Err e ->
                                                Err e

                                            Ok b ->
                                                Ok
                                                    (editorState
                                                        |> withRoot b
                                                    )


isTextBlock : Path -> Node -> Bool
isTextBlock _ node =
    case node of
        Block bn ->
            case childNodes bn of
                InlineChildren _ ->
                    True

                _ ->
                    False

        _ ->
            False


type alias FindFunc =
    (Path -> Node -> Bool) -> Path -> BlockNode -> Maybe ( Path, Node )


findTextBlock : FindFunc -> Path -> BlockNode -> Maybe ( Path, BlockNode )
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
                Block bn ->
                    Just ( p, bn )

                _ ->
                    Nothing


findNextTextBlock : Path -> BlockNode -> Maybe ( Path, BlockNode )
findNextTextBlock =
    findTextBlock findForwardFromExclusive


findPreviousTextBlock : Path -> BlockNode -> Maybe ( Path, BlockNode )
findPreviousTextBlock =
    findTextBlock findBackwardFromExclusive


removeRangeSelection : Transform
removeRangeSelection editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if isCollapsed selection then
                Err "Cannot remove contents of collapsed selection"

            else
                let
                    normalizedSelection =
                        normalize selection
                in
                if anchorNode normalizedSelection == focusNode normalizedSelection then
                    case
                        removeTextAtRange
                            (anchorNode normalizedSelection)
                            (anchorOffset normalizedSelection)
                            (Just (focusOffset normalizedSelection))
                            (State.root editorState)
                    of
                        Ok newRoot ->
                            let
                                newSelection =
                                    caretSelection (anchorNode normalizedSelection) (anchorOffset normalizedSelection)
                            in
                            Ok
                                (editorState
                                    |> withRoot newRoot
                                    |> withSelection (Just newSelection)
                                )

                        Err s ->
                            Err s

                else
                    let
                        anchorTextBlock =
                            findTextBlockNodeAncestor
                                (anchorNode normalizedSelection)
                                (State.root editorState)

                        focusTextBlock =
                            findTextBlockNodeAncestor
                                (focusNode normalizedSelection)
                                (State.root editorState)
                    in
                    case
                        removeTextAtRange (focusNode normalizedSelection)
                            0
                            (Just (focusOffset normalizedSelection))
                            (State.root editorState)
                    of
                        Err s ->
                            Err s

                        Ok removedEnd ->
                            case
                                removeTextAtRange
                                    (anchorNode normalizedSelection)
                                    (anchorOffset normalizedSelection)
                                    Nothing
                                    removedEnd
                            of
                                Err s ->
                                    Err s

                                Ok removedStart ->
                                    let
                                        removedNodes =
                                            removeInRange
                                                (increment (anchorNode normalizedSelection))
                                                (decrement (focusNode normalizedSelection))
                                                removedStart

                                        newSelection =
                                            caretSelection
                                                (anchorNode normalizedSelection)
                                                (anchorOffset normalizedSelection)

                                        newEditorState =
                                            editorState
                                                |> withRoot removedNodes
                                                |> withSelection (Just newSelection)
                                    in
                                    case anchorTextBlock of
                                        Nothing ->
                                            Ok newEditorState

                                        Just ( ap, _ ) ->
                                            case focusTextBlock of
                                                Nothing ->
                                                    Ok newEditorState

                                                Just ( fp, _ ) ->
                                                    if ap == fp then
                                                        Ok newEditorState

                                                    else
                                                        Ok <| Result.withDefault newEditorState (joinForward newEditorState)


insertLineBreak : Transform
insertLineBreak =
    insertInlineElement
        (InlineLeaf (inlineLeafParameters (elementParameters hardBreak [] Set.empty) []))


insertInlineElement : InlineLeaf -> Transform
insertInlineElement leaf editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (insertInlineElement leaf)

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "Invalid selection"

                    Just node ->
                        case node of
                            Inline il ->
                                case il of
                                    InlineLeaf _ ->
                                        case
                                            replace
                                                (anchorNode selection)
                                                (Inline leaf)
                                                (State.root editorState)
                                        of
                                            Err e ->
                                                Err e

                                            Ok newRoot ->
                                                let
                                                    newSelection =
                                                        case
                                                            findForwardFrom
                                                                (\_ n -> isSelectable n)
                                                                (anchorNode selection)
                                                                newRoot
                                                        of
                                                            Nothing ->
                                                                Nothing

                                                            Just ( p, _ ) ->
                                                                Just (caretSelection p 0)
                                                in
                                                Ok
                                                    (editorState
                                                        |> withRoot newRoot
                                                        |> withSelection newSelection
                                                    )

                                    TextLeaf tl ->
                                        let
                                            ( before, after ) =
                                                splitTextLeaf (anchorOffset selection) tl
                                        in
                                        case
                                            replaceWithFragment
                                                (anchorNode selection)
                                                (InlineLeafFragment
                                                    (Array.fromList
                                                        [ TextLeaf before, leaf, TextLeaf after ]
                                                    )
                                                )
                                                (State.root editorState)
                                        of
                                            Err e ->
                                                Err e

                                            Ok newRoot ->
                                                let
                                                    newSelection =
                                                        case
                                                            findForwardFromExclusive
                                                                (\_ n -> isSelectable n)
                                                                (anchorNode selection)
                                                                newRoot
                                                        of
                                                            Nothing ->
                                                                Nothing

                                                            Just ( p, _ ) ->
                                                                Just (caretSelection p 0)
                                                in
                                                Ok
                                                    (editorState
                                                        |> withRoot newRoot
                                                        |> withSelection newSelection
                                                    )

                            _ ->
                                Err "I can not insert an inline element in a block node"


splitTextBlock : Transform
splitTextBlock =
    splitBlock findTextBlockNodeAncestor


splitBlock : (Path -> BlockNode -> Maybe ( Path, BlockNode )) -> Transform
splitBlock ancestorFunc editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (splitBlock ancestorFunc)

            else
                case ancestorFunc (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "I cannot find a proper ancestor to split"

                    Just ( textBlockPath, textBlockNode ) ->
                        let
                            relativePath =
                                List.drop (List.length textBlockPath) (anchorNode selection)
                        in
                        case splitBlockAtPathAndOffset relativePath (anchorOffset selection) textBlockNode of
                            Nothing ->
                                Err <| "Can not split block at path " ++ toString (anchorNode selection)

                            Just ( before, after ) ->
                                case
                                    replaceWithFragment
                                        textBlockPath
                                        (BlockNodeFragment (Array.fromList [ before, after ]))
                                        (State.root editorState)
                                of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelectionPath =
                                                increment textBlockPath ++ [ 0 ]

                                            newSelection =
                                                caretSelection newSelectionPath 0
                                        in
                                        Ok
                                            (editorState
                                                |> withRoot newRoot
                                                |> withSelection (Just newSelection)
                                            )


isLeafNode : Path -> BlockNode -> Bool
isLeafNode path root =
    case nodeAt path root of
        Nothing ->
            False

        Just node ->
            case node of
                Block bn ->
                    case childNodes bn of
                        Leaf ->
                            True

                        _ ->
                            False

                Inline l ->
                    case l of
                        InlineLeaf _ ->
                            True

                        TextLeaf _ ->
                            False


removeTextAtRange : Path -> Int -> Maybe Int -> BlockNode -> Result String BlockNode
removeTextAtRange nodePath start maybeEnd root =
    case nodeAt nodePath root of
        Just node ->
            case node of
                Block _ ->
                    Err "I was expecting a text node, but instead I got a block node"

                Inline leaf ->
                    case leaf of
                        InlineLeaf _ ->
                            Err "I was expecting a text leaf, but instead I got an inline leaf"

                        TextLeaf v ->
                            let
                                textNode =
                                    case maybeEnd of
                                        Nothing ->
                                            TextLeaf
                                                (v
                                                    |> withText (String.left start (text v))
                                                )

                                        Just end ->
                                            TextLeaf
                                                (v
                                                    |> withText
                                                        (String.left start (text v)
                                                            ++ String.dropLeft end (text v)
                                                        )
                                                )
                            in
                            replace nodePath (Inline textNode) root

        Nothing ->
            Err <| "There is no node at node path " ++ toString nodePath


removeSelectedLeafElement : Transform
removeSelectedLeafElement editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot remove a block element if it is not"

            else if isLeafNode (anchorNode selection) (State.root editorState) then
                let
                    newSelection =
                        case
                            findBackwardFromExclusive
                                (\_ n -> isSelectable n)
                                (anchorNode selection)
                                (State.root editorState)
                        of
                            Nothing ->
                                Nothing

                            Just ( p, n ) ->
                                let
                                    offset =
                                        case n of
                                            Inline il ->
                                                case il of
                                                    TextLeaf t ->
                                                        String.length (text t)

                                                    _ ->
                                                        0

                                            _ ->
                                                0
                                in
                                Just (caretSelection p offset)
                in
                Ok
                    (editorState
                        |> withRoot (removeNodeAndEmptyParents (anchorNode selection) (State.root editorState))
                        |> withSelection newSelection
                    )

            else
                Err "There's no leaf node at the given selection"



-- backspace logic for text
-- offset = 0, try to delete the previous text node's text
-- offset = 1, set the text node to empty
-- other offset, allow browser to do the default behavior


backspaceText : Transform
backspaceText editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only backspace a collapsed selection"

            else if anchorOffset selection > 1 then
                {-
                   -- This would be the logic if we didn't revert to using the native behavior.
                   removeRangeSelection
                       { editorState
                           | selection =
                               Just <|
                                   singleNodeRangeSelection
                                       (anchorNode selection)
                                       ((anchorOffset selection) - 1)
                                       (anchorOffset selection)
                       }
                -}
                Err <|
                    "I use native behavior when doing backspace when the "
                        ++ "anchor offset could not result in a node change"

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "Invalid selection"

                    Just node ->
                        case node of
                            Block _ ->
                                Err "I cannot backspace a block node"

                            Inline il ->
                                case il of
                                    InlineLeaf _ ->
                                        Err "I cannot backspace text of an inline leaf"

                                    TextLeaf tl ->
                                        if anchorOffset selection == 1 then
                                            case
                                                replace (anchorNode selection)
                                                    (Inline
                                                        (TextLeaf
                                                            (tl
                                                                |> withText (String.dropLeft 1 (text tl))
                                                            )
                                                        )
                                                    )
                                                    (State.root editorState)
                                            of
                                                Err s ->
                                                    Err s

                                                Ok newRoot ->
                                                    let
                                                        newSelection =
                                                            caretSelection (anchorNode selection) 0
                                                    in
                                                    Ok
                                                        (editorState
                                                            |> withRoot newRoot
                                                            |> withSelection (Just newSelection)
                                                        )

                                        else
                                            case previous (anchorNode selection) (State.root editorState) of
                                                Nothing ->
                                                    Err "No previous node to backspace text"

                                                Just ( previousPath, previousNode ) ->
                                                    case previousNode of
                                                        Inline previousInlineLeafWrapper ->
                                                            case previousInlineLeafWrapper of
                                                                TextLeaf previousTextLeaf ->
                                                                    let
                                                                        l =
                                                                            String.length (text previousTextLeaf)

                                                                        newSelection =
                                                                            singleNodeRangeSelection previousPath l (max 0 (l - 1))
                                                                    in
                                                                    removeRangeSelection
                                                                        (editorState
                                                                            |> withSelection (Just newSelection)
                                                                        )

                                                                InlineLeaf _ ->
                                                                    Err "Cannot backspace the text of an inline leaf"

                                                        Block _ ->
                                                            Err "Cannot backspace the text of a block node"


isBlockOrInlineNodeWithMark : String -> Node -> Bool
isBlockOrInlineNodeWithMark markName node =
    case node of
        Inline il ->
            hasMarkWithName markName (marksFromInlineLeaf il)

        _ ->
            True


toggleMarkSingleInlineNode : MarkOrder -> Mark -> ToggleAction -> Transform
toggleMarkSingleInlineNode markOrder mark action editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if anchorNode selection /= focusNode selection then
                Err "I can only toggle a single inline node"

            else
                let
                    normalizedSelection =
                        normalize selection
                in
                case nodeAt (anchorNode normalizedSelection) (State.root editorState) of
                    Nothing ->
                        Err "No node at selection"

                    Just node ->
                        case node of
                            Block _ ->
                                Err "Cannot toggle a block node"

                            Inline il ->
                                let
                                    newMarks =
                                        toggle action markOrder mark (marksFromInlineLeaf il)

                                    leaves =
                                        case il of
                                            InlineLeaf leaf ->
                                                [ InlineLeaf
                                                    (leaf
                                                        |> inlineLeafParametersWithMarks newMarks
                                                    )
                                                ]

                                            TextLeaf leaf ->
                                                if
                                                    String.length (text leaf)
                                                        == focusOffset normalizedSelection
                                                        && anchorOffset normalizedSelection
                                                        == 0
                                                then
                                                    [ TextLeaf (leaf |> textLeafParametersWithMarks newMarks) ]

                                                else
                                                    let
                                                        newNode =
                                                            TextLeaf
                                                                (leaf
                                                                    |> textLeafParametersWithMarks newMarks
                                                                    |> withText
                                                                        (String.slice
                                                                            (anchorOffset normalizedSelection)
                                                                            (focusOffset normalizedSelection)
                                                                            (text leaf)
                                                                        )
                                                                )

                                                        left =
                                                            TextLeaf
                                                                (leaf
                                                                    |> withText
                                                                        (String.left
                                                                            (anchorOffset normalizedSelection)
                                                                            (text leaf)
                                                                        )
                                                                )

                                                        right =
                                                            TextLeaf
                                                                (leaf
                                                                    |> withText
                                                                        (String.dropLeft
                                                                            (focusOffset normalizedSelection)
                                                                            (text leaf)
                                                                        )
                                                                )
                                                    in
                                                    if anchorOffset normalizedSelection == 0 then
                                                        [ newNode, right ]

                                                    else if String.length (text leaf) == focusOffset normalizedSelection then
                                                        [ left, newNode ]

                                                    else
                                                        [ left, newNode, right ]

                                    path =
                                        if anchorOffset normalizedSelection == 0 then
                                            anchorNode normalizedSelection

                                        else
                                            increment (anchorNode normalizedSelection)

                                    newSelection =
                                        singleNodeRangeSelection
                                            path
                                            0
                                            (focusOffset normalizedSelection - anchorOffset normalizedSelection)
                                in
                                case
                                    replaceWithFragment
                                        (anchorNode normalizedSelection)
                                        (InlineLeafFragment <| Array.fromList leaves)
                                        (State.root editorState)
                                of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        Ok
                                            (editorState
                                                |> withSelection (Just newSelection)
                                                |> withRoot newRoot
                                            )


toggleMarkOnInlineNodes : MarkOrder -> Mark -> ToggleAction -> Transform
toggleMarkOnInlineNodes markOrder mark action editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if focusNode selection == anchorNode selection then
                toggleMarkSingleInlineNode markOrder mark Flip editorState

            else
                let
                    normalizedSelection =
                        normalize selection

                    toggleAction =
                        if action /= Flip then
                            action

                        else if
                            allRange
                                (isBlockOrInlineNodeWithMark (Mark.name mark))
                                (anchorNode normalizedSelection)
                                (focusNode normalizedSelection)
                                (State.root editorState)
                        then
                            Remove

                        else
                            Add

                    betweenRoot =
                        case next (anchorNode normalizedSelection) (State.root editorState) of
                            Nothing ->
                                State.root editorState

                            Just ( afterAnchor, _ ) ->
                                case previous (focusNode normalizedSelection) (State.root editorState) of
                                    Nothing ->
                                        State.root editorState

                                    Just ( beforeFocus, _ ) ->
                                        case
                                            indexedMap
                                                (\path node ->
                                                    if path < afterAnchor || path > beforeFocus then
                                                        node

                                                    else
                                                        case node of
                                                            Block _ ->
                                                                node

                                                            Inline _ ->
                                                                toggleMark toggleAction markOrder mark node
                                                )
                                                (Block (State.root editorState))
                                        of
                                            Block bn ->
                                                bn

                                            _ ->
                                                State.root editorState

                    modifiedEndNodeEditorState =
                        Result.withDefault (editorState |> withRoot betweenRoot) <|
                            toggleMarkSingleInlineNode
                                markOrder
                                mark
                                toggleAction
                                (editorState
                                    |> withRoot betweenRoot
                                    |> withSelection
                                        (Just
                                            (singleNodeRangeSelection
                                                (focusNode normalizedSelection)
                                                0
                                                (focusOffset normalizedSelection)
                                            )
                                        )
                                )

                    modifiedStartNodeEditorState =
                        case nodeAt (anchorNode normalizedSelection) (State.root editorState) of
                            Nothing ->
                                modifiedEndNodeEditorState

                            Just node ->
                                case node of
                                    Inline il ->
                                        let
                                            focusOffset =
                                                case il of
                                                    TextLeaf leaf ->
                                                        String.length (text leaf)

                                                    InlineLeaf _ ->
                                                        0
                                        in
                                        Result.withDefault modifiedEndNodeEditorState <|
                                            toggleMarkSingleInlineNode
                                                markOrder
                                                mark
                                                toggleAction
                                                (modifiedEndNodeEditorState
                                                    |> withSelection
                                                        (Just
                                                            (singleNodeRangeSelection
                                                                (anchorNode normalizedSelection)
                                                                (anchorOffset normalizedSelection)
                                                                focusOffset
                                                            )
                                                        )
                                                )

                                    _ ->
                                        modifiedEndNodeEditorState

                    incrementAnchorOffset =
                        anchorOffset normalizedSelection /= 0

                    anchorAndFocusHaveSameParent =
                        parent (anchorNode normalizedSelection) == parent (focusNode normalizedSelection)

                    newSelection =
                        rangeSelection
                            (if incrementAnchorOffset then
                                increment (anchorNode normalizedSelection)

                             else
                                anchorNode normalizedSelection
                            )
                            0
                            (if incrementAnchorOffset && anchorAndFocusHaveSameParent then
                                increment (focusNode normalizedSelection)

                             else
                                focusNode normalizedSelection
                            )
                            (focusOffset normalizedSelection)
                in
                Ok
                    (modifiedStartNodeEditorState
                        |> withSelection (Just newSelection)
                    )


toggleBlock : List String -> ElementParameters -> ElementParameters -> Transform
toggleBlock allowedBlocks onParams offParams editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected."

        Just selection ->
            let
                normalizedSelection =
                    normalize selection

                anchorPath =
                    findClosestBlockPath (anchorNode normalizedSelection) (State.root editorState)

                focusPath =
                    findClosestBlockPath (focusNode normalizedSelection) (State.root editorState)

                doOffBehavior =
                    allRange
                        (\node ->
                            case node of
                                Block bn ->
                                    comparableElementParameters (elementParametersFromBlockNode bn)
                                        == comparableElementParameters onParams

                                _ ->
                                    True
                        )
                        anchorPath
                        focusPath
                        (State.root editorState)

                newParams =
                    if doOffBehavior then
                        offParams

                    else
                        onParams

                newRoot =
                    case
                        indexedMap
                            (\path node ->
                                if path < anchorPath || path > focusPath then
                                    node

                                else
                                    case node of
                                        Block bn ->
                                            let
                                                p =
                                                    elementParametersFromBlockNode bn
                                            in
                                            if List.member (nameFromElementParameters p) allowedBlocks then
                                                Block (bn |> blockNodeWithElementParameters newParams)

                                            else
                                                node

                                        Inline _ ->
                                            node
                            )
                            (Block (State.root editorState))
                    of
                        Block bn ->
                            bn

                        _ ->
                            State.root editorState
            in
            Ok (editorState |> withRoot newRoot)


wrap : (BlockNode -> BlockNode) -> ElementParameters -> Transform
wrap contentsMapFunc elementParameters editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            let
                normalizedSelection =
                    normalize selection

                markedRoot =
                    annotateSelection normalizedSelection (State.root editorState)

                anchorBlockPath =
                    findClosestBlockPath (anchorNode normalizedSelection) markedRoot

                focusBlockPath =
                    findClosestBlockPath (focusNode normalizedSelection) markedRoot

                ancestor =
                    commonAncestor anchorBlockPath focusBlockPath
            in
            if ancestor == anchorBlockPath || ancestor == focusBlockPath then
                case nodeAt ancestor markedRoot of
                    Nothing ->
                        Err "I cannot find a node at selection"

                    Just node ->
                        let
                            newChildren =
                                case node of
                                    Block bn ->
                                        blockArray (Array.map contentsMapFunc (Array.fromList [ bn ]))

                                    Inline il ->
                                        inlineLeafArray (Array.fromList [ il ])

                            newNode =
                                blockNode elementParameters newChildren
                        in
                        case replace ancestor (Block newNode) markedRoot of
                            Err err ->
                                Err err

                            Ok newRoot ->
                                Ok
                                    (editorState
                                        |> withRoot (clearSelectionAnnotations newRoot)
                                        |> withSelection
                                            (selectionFromAnnotations
                                                newRoot
                                                (anchorOffset selection)
                                                (focusOffset selection)
                                            )
                                    )

            else
                case List.Extra.getAt (List.length ancestor) (anchorNode normalizedSelection) of
                    Nothing ->
                        Err "Invalid ancestor path at anchor node"

                    Just childAnchorIndex ->
                        case List.Extra.getAt (List.length ancestor) (focusNode normalizedSelection) of
                            Nothing ->
                                Err "Invalid ancestor path at focus node"

                            Just childFocusIndex ->
                                case nodeAt ancestor markedRoot of
                                    Nothing ->
                                        Err "Invalid common ancestor path"

                                    Just node ->
                                        case node of
                                            Block bn ->
                                                case childNodes bn of
                                                    BlockChildren a ->
                                                        let
                                                            newChildNode =
                                                                blockNode elementParameters
                                                                    (blockArray <|
                                                                        Array.map
                                                                            contentsMapFunc
                                                                            (Array.slice childAnchorIndex
                                                                                (childFocusIndex + 1)
                                                                                (fromBlockArray a)
                                                                            )
                                                                    )

                                                            newBlockArray =
                                                                blockArray <|
                                                                    Array.append
                                                                        (Array.append
                                                                            (Array.Extra.sliceUntil
                                                                                childAnchorIndex
                                                                                (fromBlockArray a)
                                                                            )
                                                                            (Array.fromList [ newChildNode ])
                                                                        )
                                                                        (Array.Extra.sliceFrom
                                                                            (childFocusIndex + 1)
                                                                            (fromBlockArray a)
                                                                        )

                                                            newNode =
                                                                bn |> withChildNodes newBlockArray
                                                        in
                                                        case replace ancestor (Block newNode) markedRoot of
                                                            Err s ->
                                                                Err s

                                                            Ok newRoot ->
                                                                Ok
                                                                    (editorState
                                                                        |> withRoot (clearSelectionAnnotations newRoot)
                                                                        |> withSelection
                                                                            (selectionFromAnnotations
                                                                                newRoot
                                                                                (anchorOffset selection)
                                                                                (focusOffset selection)
                                                                            )
                                                                    )

                                                    InlineChildren _ ->
                                                        Err "Cannot wrap inline elements"

                                                    Leaf ->
                                                        Err "Cannot wrap leaf elements"

                                            Inline _ ->
                                                Err "Invalid ancestor path... somehow we have an inline leaf"


selectAll : Transform
selectAll editorState =
    let
        ( fl, lastOffset ) =
            indexedFoldl
                (\path node ( firstAndLast, offset ) ->
                    if isSelectable node then
                        let
                            newOffset =
                                case node of
                                    Inline il ->
                                        case il of
                                            TextLeaf tl ->
                                                String.length (text tl)

                                            InlineLeaf _ ->
                                                0

                                    Block _ ->
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
                (Block (State.root editorState))
    in
    case fl of
        Nothing ->
            Err "Nothing is selectable"

        Just ( first, last ) ->
            Ok
                (editorState
                    |> withSelection (Just <| rangeSelection first 0 last lastOffset)
                )



-- mark each text block to lift
-- for each block, lift it out of its container if possible


liftAnnotation =
    "__lift__"


addLiftMarkToBlocksInSelection : Selection -> BlockNode -> BlockNode
addLiftMarkToBlocksInSelection selection root =
    let
        start =
            findClosestBlockPath (anchorNode selection) root

        end =
            findClosestBlockPath (focusNode selection) root
    in
    case
        indexedMap
            (\path node ->
                if path < start || path > end then
                    node

                else
                    case node of
                        Block bn ->
                            let
                                addMarker =
                                    case childNodes bn of
                                        Leaf ->
                                            True

                                        InlineChildren _ ->
                                            True

                                        _ ->
                                            False
                            in
                            if addMarker then
                                RichTextEditor.Annotation.add liftAnnotation <| Block bn

                            else
                                node

                        _ ->
                            node
            )
            (Block root)
    of
        Block bn ->
            bn

        _ ->
            root


liftConcatMapFunc : Node -> List Node
liftConcatMapFunc node =
    case node of
        Block bn ->
            case childNodes bn of
                Leaf ->
                    [ node ]

                InlineChildren _ ->
                    [ node ]

                BlockChildren a ->
                    let
                        groupedBlockNodes =
                            List.Extra.groupWhile
                                (\n1 n2 ->
                                    Set.member
                                        liftAnnotation
                                        (annotationsFromBlockNode n1)
                                        == Set.member
                                            liftAnnotation
                                            (annotationsFromBlockNode n2)
                                )
                                (Array.toList (fromBlockArray a))
                    in
                    List.map Block <|
                        List.concatMap
                            (\( n, l ) ->
                                if Set.member liftAnnotation (annotationsFromBlockNode n) then
                                    n :: l

                                else
                                    [ bn |> withChildNodes (blockArray (Array.fromList <| n :: l)) ]
                            )
                            groupedBlockNodes

        Inline _ ->
            [ node ]


lift : Transform
lift editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            let
                normalizedSelection =
                    normalize selection

                markedRoot =
                    addLiftMarkToBlocksInSelection normalizedSelection <|
                        annotateSelection normalizedSelection (State.root editorState)

                liftedRoot =
                    concatMap liftConcatMapFunc markedRoot

                newSelection =
                    selectionFromAnnotations
                        liftedRoot
                        (anchorOffset normalizedSelection)
                        (focusOffset normalizedSelection)
            in
            Ok
                (editorState
                    |> withSelection newSelection
                    |> withRoot
                        (clearAnnotations liftAnnotation <|
                            clearSelectionAnnotations liftedRoot
                        )
                )


liftEmpty : Transform
liftEmpty editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if (not <| isCollapsed selection) || anchorOffset selection /= 0 then
                Err "Can only lift empty text blocks"

            else
                let
                    p =
                        findClosestBlockPath (anchorNode selection) (State.root editorState)
                in
                case nodeAt p (State.root editorState) of
                    Nothing ->
                        Err "Invalid root path"

                    Just node ->
                        if not <| isEmptyTextBlock node then
                            Err "I cannot lift a node that is not an empty text block"

                        else if List.length p < 2 then
                            Err "I cannot lift a node that's root or an immediate child of root"

                        else
                            lift editorState


isEmptyTextBlock : Node -> Bool
isEmptyTextBlock node =
    case node of
        Block bn ->
            case childNodes bn of
                InlineChildren a ->
                    let
                        array =
                            fromInlineArray a
                    in
                    case Array.get 0 array of
                        Nothing ->
                            Array.isEmpty array

                        Just n ->
                            Array.length array
                                == 1
                                && (case n of
                                        TextLeaf t ->
                                            String.isEmpty (text t)

                                        _ ->
                                            False
                                   )

                _ ->
                    False

        Inline _ ->
            False


splitBlockHeaderToNewParagraph : List String -> ElementParameters -> Transform
splitBlockHeaderToNewParagraph headerElements paragraphElement editorState =
    case splitTextBlock editorState of
        Err s ->
            Err s

        Ok splitEditorState ->
            case State.selection splitEditorState of
                Nothing ->
                    Ok splitEditorState

                Just selection ->
                    if (not <| isCollapsed selection) || anchorOffset selection /= 0 then
                        Ok splitEditorState

                    else
                        let
                            p =
                                findClosestBlockPath
                                    (anchorNode selection)
                                    (State.root splitEditorState)
                        in
                        case nodeAt p (State.root splitEditorState) of
                            Nothing ->
                                Ok splitEditorState

                            Just node ->
                                case node of
                                    Block bn ->
                                        let
                                            parameters =
                                                elementParametersFromBlockNode bn
                                        in
                                        if
                                            List.member
                                                (nameFromElementParameters parameters)
                                                headerElements
                                                && isEmptyTextBlock node
                                        then
                                            case
                                                replace p
                                                    (Block
                                                        (bn
                                                            |> blockNodeWithElementParameters
                                                                paragraphElement
                                                        )
                                                    )
                                                    (State.root splitEditorState)
                                            of
                                                Err _ ->
                                                    Ok splitEditorState

                                                Ok newRoot ->
                                                    Ok (splitEditorState |> withRoot newRoot)

                                        else
                                            Ok splitEditorState

                                    _ ->
                                        Ok splitEditorState


insertBlockNode : BlockNode -> Transform
insertBlockNode node editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (insertBlockNode node)

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "Invalid selection"

                    Just aNode ->
                        case aNode of
                            -- if a block node is selected, then insert after the selected block
                            Block bn ->
                                case
                                    replaceWithFragment
                                        (anchorNode selection)
                                        (BlockNodeFragment (Array.fromList [ bn, node ]))
                                        (State.root editorState)
                                of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelection =
                                                if isSelectable (Block node) then
                                                    caretSelection (increment (anchorNode selection)) 0

                                                else
                                                    selection
                                        in
                                        Ok
                                            (editorState
                                                |> withSelection (Just newSelection)
                                                |> withRoot newRoot
                                            )

                            -- if an inline node is selected, then split the block and insert before
                            Inline _ ->
                                case splitTextBlock editorState of
                                    Err s ->
                                        Err s

                                    Ok splitEditorState ->
                                        insertBlockNodeBeforeSelection node splitEditorState


insertBlockNodeBeforeSelection : BlockNode -> Transform
insertBlockNodeBeforeSelection node editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only insert a block element before a collapsed selection"

            else
                let
                    markedRoot =
                        annotateSelection selection (State.root editorState)

                    closestBlockPath =
                        findClosestBlockPath (anchorNode selection) markedRoot
                in
                case nodeAt closestBlockPath markedRoot of
                    Nothing ->
                        Err "Invalid selection"

                    Just anchorNode ->
                        case anchorNode of
                            Block bn ->
                                let
                                    newFragment =
                                        if isEmptyTextBlock <| Block bn then
                                            [ node ]

                                        else
                                            [ node, bn ]
                                in
                                case
                                    replaceWithFragment
                                        closestBlockPath
                                        (BlockNodeFragment (Array.fromList newFragment))
                                        markedRoot
                                of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelection =
                                                if isSelectable (Block node) then
                                                    Just <| caretSelection closestBlockPath 0

                                                else
                                                    selectionFromAnnotations
                                                        newRoot
                                                        (anchorOffset selection)
                                                        (focusOffset selection)
                                        in
                                        Ok
                                            (editorState
                                                |> withSelection newSelection
                                                |> withRoot (clearSelectionAnnotations newRoot)
                                            )

                            -- if an inline node is selected, then split the block and insert before
                            Inline _ ->
                                Err "Invalid state! I was expecting a block node."


backspaceInlineElement : Transform
backspaceInlineElement editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only backspace an inline element if the selection is collapsed"

            else if anchorOffset selection /= 0 then
                Err "I can only backspace an inline element if the offset is 0"

            else
                let
                    decrementedPath =
                        decrement (anchorNode selection)
                in
                case nodeAt decrementedPath (State.root editorState) of
                    Nothing ->
                        Err "There is no previous inline element"

                    Just node ->
                        case node of
                            Inline il ->
                                case il of
                                    InlineLeaf _ ->
                                        case
                                            replaceWithFragment
                                                decrementedPath
                                                (InlineLeafFragment Array.empty)
                                                (State.root editorState)
                                        of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                Ok
                                                    (editorState
                                                        |> withSelection (Just <| caretSelection decrementedPath 0)
                                                        |> withRoot newRoot
                                                    )

                                    TextLeaf _ ->
                                        Err "There is no previous inline leaf element, found a text leaf"

                            Block _ ->
                                Err "There is no previous inline leaf element, found a block node"


backspaceBlockNode : Transform
backspaceBlockNode editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsBeginningOfTextBlock selection (State.root editorState) then
                Err "Cannot backspace a block element if we're not at the beginning of a text block"

            else
                let
                    blockPath =
                        findClosestBlockPath (anchorNode selection) (State.root editorState)

                    markedRoot =
                        annotateSelection selection (State.root editorState)
                in
                case previous blockPath (State.root editorState) of
                    Nothing ->
                        Err "There is no previous element to backspace"

                    Just ( path, node ) ->
                        case node of
                            Block bn ->
                                case childNodes bn of
                                    Leaf ->
                                        case replaceWithFragment path (BlockNodeFragment Array.empty) markedRoot of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                Ok
                                                    (editorState
                                                        |> withRoot (clearSelectionAnnotations newRoot)
                                                        |> withSelection
                                                            (selectionFromAnnotations
                                                                newRoot
                                                                (anchorOffset selection)
                                                                (focusOffset selection)
                                                            )
                                                    )

                                    _ ->
                                        Err "The previous element is not a block leaf"

                            Inline _ ->
                                Err "The previous element is not a block node"


groupSameTypeInlineLeaf : InlineLeaf -> InlineLeaf -> Bool
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


textFromGroup : List InlineLeaf -> String
textFromGroup leaves =
    String.join "" <|
        List.map
            (\leaf ->
                case leaf of
                    TextLeaf t ->
                        text t

                    _ ->
                        ""
            )
            leaves


lengthsFromGroup : List InlineLeaf -> List Int
lengthsFromGroup leaves =
    List.map
        (\il ->
            case il of
                TextLeaf tl ->
                    String.length (text tl)

                InlineLeaf _ ->
                    0
        )
        leaves



-- Find the inline fragment that represents connected text nodes
-- get the text in that fragment
-- translate the offset for that text
-- find where to backspace


backspaceWord : Transform
backspaceWord editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot remove a word of a range selection"

            else
                case findTextBlockNodeAncestor (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "I can only remove a word on a text leaf"

                    Just ( p, n ) ->
                        case childNodes n of
                            InlineChildren arr ->
                                let
                                    groupedLeaves =
                                        -- group text nodes together
                                        List.Extra.groupWhile
                                            groupSameTypeInlineLeaf
                                            (Array.toList (fromInlineArray arr))
                                in
                                case List.Extra.last (anchorNode selection) of
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
                                                offsetUpToNewIndex + anchorOffset selection

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
                                                    rangeSelection
                                                        (p ++ [ newIndex ])
                                                        newOffset
                                                        (anchorNode selection)
                                                        (anchorOffset selection)

                                                newState =
                                                    editorState |> withSelection (Just newSelection)
                                            in
                                            removeRangeSelection newState

                            _ ->
                                Err "I expected an inline leaf array"


deleteText : Transform
deleteText editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only backspace a collapsed selection"

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "I was given an invalid path to delete text"

                    Just node ->
                        case node of
                            Block _ ->
                                Err "I cannot delete text if the selection a block node"

                            Inline il ->
                                case il of
                                    InlineLeaf _ ->
                                        Err "I cannot delete text if the selection an inline leaf"

                                    TextLeaf tl ->
                                        let
                                            textLength =
                                                String.length (text tl)
                                        in
                                        if anchorOffset selection < (textLength - 1) then
                                            Err "I use the default behavior when deleting text when the anchor offset is not at the end of a text node"

                                        else if anchorOffset selection == (textLength - 1) then
                                            case
                                                replace
                                                    (anchorNode selection)
                                                    (Inline
                                                        (TextLeaf
                                                            (tl |> withText (String.dropRight 1 (text tl)))
                                                        )
                                                    )
                                                    (State.root editorState)
                                            of
                                                Err s ->
                                                    Err s

                                                Ok newRoot ->
                                                    Ok (editorState |> withRoot newRoot)

                                        else
                                            case next (anchorNode selection) (State.root editorState) of
                                                Nothing ->
                                                    Err "I cannot do delete because there is no neighboring text node"

                                                Just ( nextPath, nextNode ) ->
                                                    case nextNode of
                                                        Block _ ->
                                                            Err "Cannot delete the text of a block node"

                                                        Inline nextInlineLeafWrapper ->
                                                            case nextInlineLeafWrapper of
                                                                TextLeaf _ ->
                                                                    let
                                                                        newSelection =
                                                                            singleNodeRangeSelection nextPath 0 1
                                                                    in
                                                                    removeRangeSelection
                                                                        (editorState
                                                                            |> withSelection (Just newSelection)
                                                                        )

                                                                InlineLeaf _ ->
                                                                    Err "Cannot backspace the text of an inline leaf"


deleteInlineElement : Transform
deleteInlineElement editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only delete an inline element if the selection is collapsed"

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "I was given an invalid path to delete text"

                    Just node ->
                        case node of
                            Block _ ->
                                Err "I cannot delete text if the selection a block node"

                            Inline il ->
                                let
                                    length =
                                        case il of
                                            TextLeaf t ->
                                                String.length (text t)

                                            InlineLeaf _ ->
                                                0
                                in
                                if length < anchorOffset selection then
                                    Err "I cannot delete an inline element if the cursor is not at the end of an inline node"

                                else
                                    let
                                        incrementedPath =
                                            increment (anchorNode selection)
                                    in
                                    case nodeAt incrementedPath (State.root editorState) of
                                        Nothing ->
                                            Err "There is no next inline leaf to delete"

                                        Just incrementedNode ->
                                            case incrementedNode of
                                                Inline nil ->
                                                    case nil of
                                                        InlineLeaf _ ->
                                                            case
                                                                replaceWithFragment
                                                                    incrementedPath
                                                                    (InlineLeafFragment Array.empty)
                                                                    (State.root editorState)
                                                            of
                                                                Err s ->
                                                                    Err s

                                                                Ok newRoot ->
                                                                    Ok (editorState |> withRoot newRoot)

                                                        TextLeaf _ ->
                                                            Err "There is no next inline leaf element, found a text leaf"

                                                Block _ ->
                                                    Err "There is no next inline leaf, found a block node"


deleteBlockNode : Transform
deleteBlockNode editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsEndOfTextBlock selection (State.root editorState) then
                Err "Cannot delete a block element if we're not at the end of a text block"

            else
                case next (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "There is no next node to delete"

                    Just ( path, node ) ->
                        case node of
                            Block bn ->
                                case childNodes bn of
                                    Leaf ->
                                        case
                                            replaceWithFragment
                                                path
                                                (BlockNodeFragment Array.empty)
                                                (State.root editorState)
                                        of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                Ok <| (editorState |> withRoot (clearSelectionAnnotations newRoot))

                                    _ ->
                                        Err "The next node is not a block leaf"

                            Inline _ ->
                                Err "The next node is not a block leaf, it's an inline leaf"


deleteWord : Transform
deleteWord editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot remove a word of a range selection"

            else
                case findTextBlockNodeAncestor (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "I can only remove a word on a text leaf"

                    Just ( p, n ) ->
                        case childNodes n of
                            InlineChildren arr ->
                                let
                                    groupedLeaves =
                                        List.Extra.groupWhile
                                            groupSameTypeInlineLeaf
                                            (Array.toList (fromInlineArray arr))
                                in
                                case List.Extra.last (anchorNode selection) of
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
                                                offsetUpToNewIndex + anchorOffset selection

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
                                                    rangeSelection
                                                        (p ++ [ newIndex ])
                                                        newOffset
                                                        (anchorNode selection)
                                                        (anchorOffset selection)

                                                newState =
                                                    editorState |> withSelection (Just newSelection)
                                            in
                                            removeRangeSelection newState

                            _ ->
                                Err "I expected an inline leaf array"
