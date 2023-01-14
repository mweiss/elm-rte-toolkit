module RichText.Commands exposing
    ( defaultCommandMap, defaultInputEventCommand, defaultKeyCommand
    , removeRange, removeRangeAndInsert, removeSelectedLeafElement
    , backspaceBlock, backspaceInlineElement, backspaceText, backspaceWord, selectBackward
    , deleteBlock, deleteInlineElement, deleteText, deleteWord, selectForward
    , insertBlock, insertInline, insertLineBreak, insertText, insertNewline, insertAfterBlockLeaf
    , joinBackward, joinForward
    , lift, liftEmpty
    , splitBlock, splitBlockHeaderToNewParagraph, splitTextBlock
    , toggleTextBlock, toggleMark
    , selectAll
    , wrap
    )

{-| This module contains pre defined commands and transforms, which are the building blocks for
modifying the editor.


# Commands

@docs defaultCommandMap, defaultInputEventCommand, defaultKeyCommand


# Transforms


## Remove selection

@docs removeRange, removeRangeAndInsert, removeSelectedLeafElement


## Backspace

@docs backspaceBlock, backspaceInlineElement, backspaceText, backspaceWord, selectBackward


## Deletion

@docs deleteBlock, deleteInlineElement, deleteText, deleteWord, selectForward


## Insert

@docs insertBlock, insertInline, insertLineBreak, insertText, insertNewline, insertAfterBlockLeaf


## Join

@docs joinBackward, joinForward


## Lift

@docs lift, liftEmpty


## Split

@docs splitBlock, splitBlockHeaderToNewParagraph, splitTextBlock


## Toggle

Toggle commands for elements and marks

@docs toggleTextBlock, toggleMark


## Selection

@docs selectAll


## Wrap

@docs wrap

-}

import Array exposing (Array)
import Array.Extra
import List.Extra
import Regex
import RichText.Annotation as Annotation exposing (annotateSelection, clear, clearSelectionAnnotations, doLift, isSelectable, selectionFromAnnotations)
import RichText.Config.Command
    exposing
        ( CommandBinding
        , CommandMap
        , InternalAction(..)
        , NamedCommandList
        , Transform
        , emptyCommandMap
        , inputEvent
        , internal
        , key
        , set
        , transform
        , withDefaultInputEventCommand
        , withDefaultKeyCommand
        )
import RichText.Config.Keys exposing (alt, backspace, delete, enter, return, shift, short)
import RichText.Definitions exposing (hardBreak)
import RichText.Internal.DeleteWord as DeleteWord
import RichText.Internal.Event exposing (InputEvent, KeyboardEvent)
import RichText.Model.Element as Element exposing (Element)
import RichText.Model.InlineElement as InlineElement
import RichText.Model.Mark as Mark exposing (Mark, MarkOrder, ToggleAction(..), hasMarkWithName, toggle)
import RichText.Model.Node as Node exposing (Block, BlockChildren, Children(..), Inline(..), InlineChildren, Path, block, blockChildren, childNodes, commonAncestor, decrement, increment, inlineChildren, marks, parent, plainText, toBlockArray, toInlineArray, toString, withChildNodes, withElement)
import RichText.Model.Selection
    exposing
        ( Selection
        , anchorNode
        , anchorOffset
        , caret
        , focusNode
        , focusOffset
        , isCollapsed
        , normalize
        , range
        , singleNodeRange
        )
import RichText.Model.State as State exposing (State, withRoot, withSelection)
import RichText.Model.Text as Text
import RichText.Node as RTNode
    exposing
        ( Fragment(..)
        , Node(..)
        , allRange
        , findBackwardFromExclusive
        , findClosestBlockPath
        , findForwardFrom
        , findForwardFromExclusive
        , findTextBlockNodeAncestor
        , indexedFoldl
        , indexedMap
        , insertAfter
        , isEmptyTextBlock
        , joinBlocks
        , next
        , nodeAt
        , previous
        , removeInRange
        , removeNodeAndEmptyParents
        , replace
        , replaceWithFragment
        , selectionIsBeginningOfTextBlock
        , selectionIsEndOfTextBlock
        , splitBlockAtPathAndOffset
        , splitTextLeaf
        )
import RichText.State exposing (translateReducedTextBlockSelection)


backspaceCommands =
    [ ( "removeRange", transform removeRange )
    , ( "removeSelectedLeafElementCommand", transform removeSelectedLeafElement )
    , ( "backspaceInlineElement", transform backspaceInlineElement )
    , ( "backspaceBlock", transform backspaceBlock )
    , ( "joinBackward", transform joinBackward )
    , ( "selectBackward", transform selectBackward )
    ]


deleteCommands =
    [ ( "removeRange", transform removeRange )
    , ( "removeSelectedLeafElementCommand", transform removeSelectedLeafElement )
    , ( "deleteInlineElement", transform deleteInlineElement )
    , ( "deleteBlock", transform deleteBlock )
    , ( "joinForward", transform joinForward )
    , ( "selectForward", transform selectForward )
    ]


{-| A starting point for creating your own command map. Contains deletion, line break, lift,
split, select all, and undo/redo behavior.
-}
defaultCommandMap : CommandMap
defaultCommandMap =
    emptyCommandMap
        |> set
            [ inputEvent "insertLineBreak", key [ shift, enter ], key [ shift, return ] ]
            [ ( "insertLineBreak", transform insertLineBreak ) ]
        |> set [ inputEvent "insertParagraph", key [ enter ], key [ return ] ]
            [ ( "liftEmpty", transform liftEmpty ), ( "splitTextBlock", transform splitTextBlock ) ]
        |> set [ inputEvent "deleteContentBackward", key [ backspace ] ]
            (backspaceCommands ++ [ ( "backspaceText", transform backspaceText ) ])
        |> set [ inputEvent "deleteWordBackward", key [ alt, backspace ] ]
            (backspaceCommands ++ [ ( "backspaceWord", transform backspaceWord ) ])
        |> set [ inputEvent "deleteContentForward", key [ delete ] ]
            (deleteCommands ++ [ ( "deleteText", transform deleteText ) ])
        |> set [ inputEvent "deleteWordForward", key [ alt, delete ] ]
            (deleteCommands ++ [ ( "deleteWord", transform deleteWord ) ])
        |> set [ key [ short, "a" ] ]
            [ ( "selectAll", transform selectAll ) ]
        |> set [ inputEvent "historyUndo", key [ short, "z" ] ]
            [ ( "undo", internal Undo ) ]
        |> set [ inputEvent "historyRedo", key [ short, shift, "z" ] ]
            [ ( "redo", internal Redo ) ]
        |> withDefaultKeyCommand defaultKeyCommand
        |> withDefaultInputEventCommand defaultInputEventCommand


{-| The default key command does remove range when a range is selected and a regular
key is pressed. In this case, we want to remove range and insert the character related to that key.
-}
defaultKeyCommand : KeyboardEvent -> NamedCommandList
defaultKeyCommand event =
    if not event.altKey && not event.metaKey && not event.ctrlKey && String.length event.key == 1 then
        [ ( "removeRangeAndInsert", transform <| removeRangeAndInsert event.key ) ]

    else
        []


{-| The default input event command does remove range when a range is selected and an insertText
event occurs. In this case, we want to remove range and insert the text related to the input
data.
-}
defaultInputEventCommand : InputEvent -> NamedCommandList
defaultInputEventCommand event =
    if event.inputType == "insertText" then
        case event.data of
            Nothing ->
                []

            Just data ->
                [ ( "removeRangeAndInsert", transform <| removeRangeAndInsert data ) ]

    else
        []


{-| Removes the contents in the state's range selection and optionally
inserts the given text if possible. Returns an error if the selection is collapsed or
it cannot remove the selection.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "hello"
                                    , inlineElement (Element.element image []) []
                                    , plainText "world"
                                    ]
                            )
                        ]
                )
            )
            (Just <| range [ 0, 0 ] 2 [ 0, 2 ] 2)

    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "het"
                                    , plainText "rld"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 3)

    removeRangeAndInsert "t" before == Ok after
    --> True

-}
removeRangeAndInsert : String -> Transform
removeRangeAndInsert s editorState =
    Result.map
        (\removedRangeEditorState ->
            Result.withDefault
                removedRangeEditorState
                (insertText s removedRangeEditorState)
        )
        (removeRange editorState)


{-| Insert a substring at the specified index. (derived from String.Extra)

    insertAt "world" 6 "Hello " == "Hello world"

-}
insertAt : String -> Int -> String -> String
insertAt insert pos string =
    String.slice 0 pos string ++ insert ++ String.slice pos (String.length string) string


{-| Inserts text at the state's selection, otherwise returns an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 2)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "teinsertxt" ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 8)

    insertText before == Ok after
    --> True

-}
insertText : String -> Transform
insertText s editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRange editorState |> Result.andThen (insertText s)

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "Invalid selection after remove range"

                    Just node ->
                        case node of
                            Block _ ->
                                Err "I was expecting a text leaf, but instead I found a block node"

                            Inline il ->
                                case il of
                                    InlineElement _ ->
                                        Err "I was expecting a text leaf, but instead found an inline element"

                                    Text tl ->
                                        let
                                            newText =
                                                insertAt s (anchorOffset selection) (Text.text tl)

                                            newTextLeaf =
                                                Text (tl |> Text.withText newText)
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
                                                                caret
                                                                    (anchorNode selection)
                                                                    (anchorOffset selection + String.length s)
                                                            )
                                                    )


{-| If the selection is collapsed and at the start of a text block, tries to join the current
block the previous one. Otherwise, returns an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    ]
                            )
                        , block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text2"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 1, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    , plainText "text2"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 4)

    joinBackward before == Ok after
    --> True

-}
joinBackward : Transform
joinBackward editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsBeginningOfTextBlock selection (State.root editorState) then
                Err "I can only join backward if the selection is at beginning of a text block"

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
                                                toInlineArray a
                                        in
                                        case Array.get (Array.length array - 1) array of
                                            Nothing ->
                                                Err "There must be at least one element in the inline node to join with"

                                            Just leaf ->
                                                let
                                                    newSelection =
                                                        case leaf of
                                                            Text tl ->
                                                                caret
                                                                    (p ++ [ Array.length array - 1 ])
                                                                    (String.length (Text.text tl))

                                                            InlineElement _ ->
                                                                caret
                                                                    (p ++ [ Array.length array - 1 ])
                                                                    0
                                                in
                                                joinForward
                                                    (editorState
                                                        |> withSelection (Just newSelection)
                                                    )

                                    _ ->
                                        Err "I can only join with text blocks"


{-| If the selection is collapsed and at the end of a text block, tries to join the current
block the next one. Otherwise, returns an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "text" ])
                        , block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "text2" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 4)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    , plainText "text2"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 4)

    joinForward before == Ok after
    --> True

-}
joinForward : Transform
joinForward editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| selectionIsEndOfTextBlock selection (State.root editorState) then
                Err "I can only join forward if the selection is at end of a text block"

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
                                                ++ Node.toString p1
                                                ++ " ,"
                                                ++ Node.toString p2

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
    (Path -> Node -> Bool) -> Path -> Block -> Maybe ( Path, Node )


findTextBlock : FindFunc -> Path -> Block -> Maybe ( Path, Block )
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


findNextTextBlock : Path -> Block -> Maybe ( Path, Block )
findNextTextBlock =
    findTextBlock findForwardFromExclusive


findPreviousTextBlock : Path -> Block -> Maybe ( Path, Block )
findPreviousTextBlock =
    findTextBlock findBackwardFromExclusive


{-| Delete the nodes in the selection, if there is one. Succeeds if the selection is a range
selection and a successful remove operation occurred, otherwise returns the error describing why
removing the nodes failed.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "hello"
                                    , inlineElement (Element.element image []) []
                                    , plainText "world"
                                    ]
                            )
                        ]
                )
            )
            (Just <| range [ 0, 0 ] 2 [ 0, 2 ] 2)

    after : State
    after =
       state
           (block
               (Element.element doc [])
               (blockChildren <|
                   Array.fromList
                       [ block
                           (Element.element paragraph [])
                           (inlineChildren <|
                               Array.fromList
                                   [ plainText "he"
                                   , plainText "rld"
                                   ]
                           )
                       ]
               )
           )
           (Just <| caret [ 0, 0 ] 2)

    removeRange before == Ok after
    --> True

-}
removeRange : Transform
removeRange editorState =
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
                        removeNodeOrTextWithRange
                            (anchorNode normalizedSelection)
                            (anchorOffset normalizedSelection)
                            (Just (focusOffset normalizedSelection))
                            (State.root editorState)
                    of
                        Ok ( newRoot, _ ) ->
                            let
                                newSelection =
                                    caret (anchorNode normalizedSelection) (anchorOffset normalizedSelection)
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
                        if focusOffset normalizedSelection == 0 then
                            Ok ( State.root editorState, Nothing )

                        else
                            removeNodeOrTextWithRange (focusNode normalizedSelection)
                                0
                                (Just (focusOffset normalizedSelection))
                                (State.root editorState)
                    of
                        Err s ->
                            Err s

                        Ok ( removedEnd, _ ) ->
                            let
                                removedNodes =
                                    removeInRange
                                        (increment (anchorNode normalizedSelection))
                                        (decrement (focusNode normalizedSelection))
                                        removedEnd
                            in
                            case
                                removeNodeOrTextWithRange
                                    (anchorNode normalizedSelection)
                                    (anchorOffset normalizedSelection)
                                    Nothing
                                    removedNodes
                            of
                                Err s ->
                                    Err s

                                Ok ( removedStart, maybePath ) ->
                                    let
                                        newSelection =
                                            Maybe.map
                                                (\( p, n ) ->
                                                    let
                                                        offset =
                                                            case n of
                                                                Inline i ->
                                                                    case i of
                                                                        Text t ->
                                                                            String.length <| Text.text t

                                                                        _ ->
                                                                            0

                                                                _ ->
                                                                    0
                                                    in
                                                    caret
                                                        p
                                                        offset
                                                )
                                                maybePath

                                        defaultedSelection =
                                            case newSelection of
                                                Nothing ->
                                                    Maybe.map
                                                        (\( p, _ ) -> caret p 0)
                                                        (findForwardFrom (\_ n -> isSelectable n) [] removedStart)

                                                Just _ ->
                                                    newSelection

                                        newEditorState =
                                            editorState
                                                |> withRoot removedStart
                                                |> withSelection defaultedSelection
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


{-| Inserts a hard break at the current selection.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 2)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "te"
                                    , inlineElement (Element.element hardBreak []) []
                                    , plainText "xt"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 2 ] 0)

    insertLineBreak before == Ok after
    --> True

-}
insertLineBreak : Transform
insertLineBreak =
    insertInline
        (Node.inlineElement (Element.element hardBreak []) [])


{-| Inserts the inline at the current selection. If the inline is selectable,
it selects it at offset 0, otherwise the selection becomes the next selectable item if it exists.
Returns an error if it cannot insert.

    img : Inline
    img =
        inlineElement (Element.element image []) []

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 2)

    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "te"
                                    , img
                                    , plainText "xt"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 1 ] 0)

    insertInline before == Ok after
    --> True

-}
insertInline : Inline -> Transform
insertInline leaf editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRange editorState |> Result.andThen (insertInline leaf)

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "Invalid selection"

                    Just node ->
                        case node of
                            Inline il ->
                                case il of
                                    InlineElement _ ->
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
                                                                Just (caret p 0)
                                                in
                                                Ok
                                                    (editorState
                                                        |> withRoot newRoot
                                                        |> withSelection newSelection
                                                    )

                                    Text tl ->
                                        let
                                            ( before, after ) =
                                                splitTextLeaf (anchorOffset selection) tl
                                        in
                                        case
                                            replaceWithFragment
                                                (anchorNode selection)
                                                (InlineFragment
                                                    (Array.fromList
                                                        [ Text before, leaf, Text after ]
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
                                                                Just (caret p 0)
                                                in
                                                Ok
                                                    (editorState
                                                        |> withRoot newRoot
                                                        |> withSelection newSelection
                                                    )

                            _ ->
                                Err "I can not insert an inline element if a block is selected"


{-| Same as `splitBlock` but the searches for a text block ancestor if one exists.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        , block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        ]
                )
            )
            (Just <| caret [ 1, 0 ] 0)

    splitTextBlock before == Ok after
    --> True

-}
splitTextBlock : Transform
splitTextBlock =
    splitBlock findTextBlockNodeAncestor


{-| Split the ancestor block determined by the passed in function of the selection.
If the selection is a range selection, also delete its content.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        , block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        ]
                )
            )
            (Just <| caret [ 1, 0 ] 0)

    splitBlock findTextBlockAncestor before == Ok after
    --> True

-}
splitBlock : (Path -> Block -> Maybe ( Path, Block )) -> Transform
splitBlock ancestorFunc editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRange editorState |> Result.andThen (splitBlock ancestorFunc)

            else
                case ancestorFunc (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "I cannot find a proper ancestor to split"

                    Just ( ancestorPath, ancestorNode ) ->
                        let
                            relativePath =
                                List.drop (List.length ancestorPath) (anchorNode selection)
                        in
                        case splitBlockAtPathAndOffset relativePath (anchorOffset selection) ancestorNode of
                            Nothing ->
                                Err <| "Can not split block at path " ++ toString (anchorNode selection)

                            Just ( before, after ) ->
                                case
                                    replaceWithFragment
                                        ancestorPath
                                        (BlockFragment (Array.fromList [ before, after ]))
                                        (State.root editorState)
                                of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelectionPath =
                                                increment ancestorPath
                                                    ++ List.repeat
                                                        (List.length (anchorNode selection) - List.length ancestorPath)
                                                        0

                                            newSelection =
                                                caret newSelectionPath 0
                                        in
                                        Ok
                                            (editorState
                                                |> withRoot newRoot
                                                |> withSelection (Just newSelection)
                                            )


isLeafNode : Path -> Block -> Bool
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
                        InlineElement _ ->
                            True

                        Text _ ->
                            False


{-| This is a helper method to remove a node or some text in a given range. If this is a node,
it returns the previously selectable node. Otherwise, it re
-}
removeNodeOrTextWithRange : Path -> Int -> Maybe Int -> Block -> Result String ( Block, Maybe ( Path, Node ) )
removeNodeOrTextWithRange nodePath start maybeEnd root =
    case nodeAt nodePath root of
        Just node ->
            case node of
                Block _ ->
                    let
                        previouslySelectablePathAndNode =
                            findBackwardFromExclusive (\_ n -> isSelectable n) nodePath root

                        newRoot =
                            removeNodeAndEmptyParents nodePath root
                    in
                    Ok ( newRoot, previouslySelectablePathAndNode )

                Inline leaf ->
                    case leaf of
                        InlineElement _ ->
                            let
                                previouslySelectablePath =
                                    findBackwardFromExclusive (\_ n -> isSelectable n) nodePath root

                                newRoot =
                                    removeNodeAndEmptyParents nodePath root
                            in
                            Ok ( newRoot, previouslySelectablePath )

                        Text v ->
                            let
                                textNode =
                                    case maybeEnd of
                                        Nothing ->
                                            Text
                                                (v
                                                    |> Text.withText (String.left start (Text.text v))
                                                )

                                        Just end ->
                                            Text
                                                (v
                                                    |> Text.withText
                                                        (String.left start (Text.text v)
                                                            ++ String.dropLeft end (Text.text v)
                                                        )
                                                )
                            in
                            Result.map (\r -> ( r, Just ( nodePath, Inline textNode ) )) (replace nodePath (Inline textNode) root)

        Nothing ->
            Err <| "There is no node at node path " ++ toString nodePath


{-| Removes a leaf element if it is the selected element, otherwise fails with an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "hello"
                                    , inlineElement (Element.element image []) []
                                    , plainText "world"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 1 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "hello"
                                    , plainText "world"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 5)

    removeSelectedLeafElement before == Ok after
    --> True

-}
removeSelectedLeafElement : Transform
removeSelectedLeafElement editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot remove a leaf element if it is not collapsed"

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
                                                    Text t ->
                                                        String.length (Text.text t)

                                                    _ ->
                                                        0

                                            _ ->
                                                0
                                in
                                Just (caret p offset)
                in
                Ok
                    (editorState
                        |> withRoot (removeNodeAndEmptyParents (anchorNode selection) (State.root editorState))
                        |> withSelection newSelection
                    )

            else
                Err "There's no leaf node at the given selection"


{-| Backspace transform for a single character. This function has a few quirks in order to take
advantage of native backspace behavior, namely:

  - selection offset = 0, try to delete the previous text node's text
  - selection offset = 1, remove the first character (afterwards, the reduce behavior of `apply`
    may remove the text node)
  - any other offset, return an error to allow browser to do the default behavior

```
before : State
before =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "text"
                                , markedText "text2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)

after : State
after =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "tex"
                                , markedText "text2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 3)

backspaceText before == Ok after
--> True
```

-}
backspaceText : Transform
backspaceText editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only backspace a collapsed selection"

            else if anchorOffset selection > 1 then
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
                                    InlineElement _ ->
                                        Err "I cannot backspace text of an inline leaf"

                                    Text tl ->
                                        if anchorOffset selection == 1 then
                                            case
                                                replace (anchorNode selection)
                                                    (Inline
                                                        (Text
                                                            (tl
                                                                |> Text.withText (String.dropLeft 1 (Text.text tl))
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
                                                            caret (anchorNode selection) 0
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
                                                                Text previousTextLeaf ->
                                                                    let
                                                                        l =
                                                                            String.length (Text.text previousTextLeaf)

                                                                        newSelection =
                                                                            singleNodeRange previousPath l (max 0 (l - 1))
                                                                    in
                                                                    removeRange
                                                                        (editorState
                                                                            |> withSelection (Just newSelection)
                                                                        )

                                                                InlineElement _ ->
                                                                    Err "Cannot backspace if the previous node is an inline leaf"

                                                        Block _ ->
                                                            Err "Cannot backspace if the previous node is a block"


isBlockOrInlineNodeWithMark : String -> Node -> Bool
isBlockOrInlineNodeWithMark markName node =
    case node of
        Inline il ->
            hasMarkWithName markName (marks il)

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
                                        toggle action markOrder mark (marks il)

                                    leaves =
                                        case il of
                                            InlineElement leaf ->
                                                [ InlineElement
                                                    (leaf
                                                        |> InlineElement.withMarks newMarks
                                                    )
                                                ]

                                            Text leaf ->
                                                if
                                                    String.length (Text.text leaf)
                                                        == focusOffset normalizedSelection
                                                        && anchorOffset normalizedSelection
                                                        == 0
                                                then
                                                    [ Text (leaf |> Text.withMarks newMarks) ]

                                                else
                                                    let
                                                        newNode =
                                                            Text
                                                                (leaf
                                                                    |> Text.withMarks newMarks
                                                                    |> Text.withText
                                                                        (String.slice
                                                                            (anchorOffset normalizedSelection)
                                                                            (focusOffset normalizedSelection)
                                                                            (Text.text leaf)
                                                                        )
                                                                )

                                                        left =
                                                            Text
                                                                (leaf
                                                                    |> Text.withText
                                                                        (String.left
                                                                            (anchorOffset normalizedSelection)
                                                                            (Text.text leaf)
                                                                        )
                                                                )

                                                        right =
                                                            Text
                                                                (leaf
                                                                    |> Text.withText
                                                                        (String.dropLeft
                                                                            (focusOffset normalizedSelection)
                                                                            (Text.text leaf)
                                                                        )
                                                                )
                                                    in
                                                    if anchorOffset normalizedSelection == 0 then
                                                        [ newNode, right ]

                                                    else if String.length (Text.text leaf) == focusOffset normalizedSelection then
                                                        [ left, newNode ]

                                                    else
                                                        [ left, newNode, right ]

                                    path =
                                        if anchorOffset normalizedSelection == 0 then
                                            anchorNode normalizedSelection

                                        else
                                            increment (anchorNode normalizedSelection)

                                    newSelection =
                                        singleNodeRange
                                            path
                                            0
                                            (focusOffset normalizedSelection - anchorOffset normalizedSelection)
                                in
                                case
                                    replaceWithFragment
                                        (anchorNode normalizedSelection)
                                        (InlineFragment <| Array.fromList leaves)
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


hugLeft : State -> State
hugLeft state =
    case State.selection state of
        Nothing ->
            state

        Just selection ->
            if isCollapsed selection then
                state

            else
                let
                    normalizedSelection =
                        normalize selection

                    anchorPath =
                        anchorNode normalizedSelection

                    root =
                        State.root state
                in
                case nodeAt anchorPath root of
                    Nothing ->
                        state

                    Just n ->
                        case n of
                            Inline il ->
                                case il of
                                    Text t ->
                                        if String.length (Text.text t) == anchorOffset normalizedSelection then
                                            case nodeAt (increment anchorPath) root of
                                                Nothing ->
                                                    state

                                                Just n2 ->
                                                    case n2 of
                                                        Inline il2 ->
                                                            case il2 of
                                                                Text _ ->
                                                                    state
                                                                        |> withSelection
                                                                            (Just <| range (increment anchorPath) 0 (focusNode normalizedSelection) (focusOffset normalizedSelection))

                                                                _ ->
                                                                    state

                                                        _ ->
                                                            state

                                        else
                                            state

                                    _ ->
                                        state

                            _ ->
                                state


hugRight : State -> State
hugRight state =
    case State.selection state of
        Nothing ->
            state

        Just selection ->
            if isCollapsed selection then
                state

            else
                let
                    normalizedSelection =
                        normalize selection

                    focusPath =
                        focusNode normalizedSelection

                    root =
                        State.root state
                in
                case nodeAt focusPath root of
                    Nothing ->
                        state

                    Just n ->
                        case n of
                            Inline il ->
                                case il of
                                    Text _ ->
                                        if 0 == focusOffset normalizedSelection then
                                            case nodeAt (decrement focusPath) root of
                                                Nothing ->
                                                    state

                                                Just n2 ->
                                                    case n2 of
                                                        Inline il2 ->
                                                            case il2 of
                                                                Text t ->
                                                                    state
                                                                        |> withSelection
                                                                            (Just <|
                                                                                range (anchorNode normalizedSelection)
                                                                                    (anchorOffset normalizedSelection)
                                                                                    (decrement focusPath)
                                                                                    (String.length (Text.text t))
                                                                            )

                                                                _ ->
                                                                    state

                                                        _ ->
                                                            state

                                        else
                                            state

                                    _ ->
                                        state

                            _ ->
                                state


{-| Sometimes a selection is right on the outside of a text node, which can confuse the toggle logic.
This method hugs the selection by pushing the normalized anchor and focus to the closest neighbor if the anchor offset is at the
end of a text node or a the focus is at the beginning.
-}
hug : State -> State
hug state =
    hugRight <| hugLeft state


{-| Applies the toggle action for the mark. The affected marks are sorted by the given mark order.

    boldMark : Mark
    boldMark =
        mark bold []


    markdownMarkOrder : MarkOrder
    markdownMarkOrder =
        markOrderFromSpec markdown


    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "text" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ markedText "" [ boldMark ], plainText "text" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)

    toggleMark markdownMarkOrder boldMark Add example == Ok

-}
toggleMark : MarkOrder -> Mark -> ToggleAction -> Transform
toggleMark markOrder mark action editorState =
    toggleMarkFull markOrder mark action (hug editorState)


toggleMarkFull : MarkOrder -> Mark -> ToggleAction -> Transform
toggleMarkFull markOrder mark action editorState =
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
                                                                RTNode.toggleMark toggleAction markOrder mark node
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
                                            (singleNodeRange
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
                                                    Text leaf ->
                                                        String.length (Text.text leaf)

                                                    InlineElement _ ->
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
                                                            (singleNodeRange
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
                        range
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


{-| Changes the selected block or blocks to the onElement if one or more blocks is not that
element. Otherwise, it changes it to the off element. If an element is not in the allowedElements,
then it is unaffected.

The arguments are as follows:

  - `onElement` - The element to change to if there is one or more block that is not that element
  - `offElement` - The element to change to if all blocks are the `onElement`
  - `convertToPlainText` - if true, strips the inline content of all marks and inline elements.

```
before : State
before =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


after : State
after =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element heading [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)

toggleTextBlock (Element.element heading []) (Element.element paragraph []) False before == Ok after
```

-}
toggleTextBlock : Element -> Element -> Bool -> Transform
toggleTextBlock onElement offElement convertToPlainText editorState =
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
                                    case childNodes bn of
                                        InlineChildren _ ->
                                            Node.element bn == onElement

                                        _ ->
                                            True

                                _ ->
                                    True
                        )
                        anchorPath
                        focusPath
                        (State.root editorState)

                newParams =
                    if doOffBehavior then
                        offElement

                    else
                        onElement

                newRoot =
                    case
                        indexedMap
                            (\path node ->
                                if path < anchorPath || path > focusPath then
                                    node

                                else
                                    case node of
                                        Block bn ->
                                            case childNodes bn of
                                                InlineChildren ic ->
                                                    let
                                                        newInlineChildren =
                                                            if convertToPlainText then
                                                                inlineChildren (Array.fromList [ plainText (convertInlineChildrenToString ic) ])

                                                            else
                                                                InlineChildren ic
                                                    in
                                                    Block (bn |> withElement newParams |> withChildNodes newInlineChildren)

                                                _ ->
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
            if convertToPlainText then
                Ok <| translateReducedTextBlockSelection newRoot editorState

            else
                Ok (editorState |> withRoot newRoot)


convertInlineChildrenToString : InlineChildren -> String
convertInlineChildrenToString ic =
    Array.foldl
        (\i s ->
            case i of
                Text t ->
                    s ++ Text.text t

                _ ->
                    s
        )
        ""
        (toInlineArray ic)


{-| Wraps the selection in the given element. The first argument is a mapping function for each
block element affected by the wrap. For normal transforms, using `identity` is okay, but for things
like lists, you may want to apply a function to wrap an additional list item block around each node.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "text" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element blockquote [])
                            (blockChildren <|
                                Array.fromList
                                    [ block
                                        (Element.element paragraph [])
                                        (inlineChildren <| Array.fromList [ plainText "text" ])
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 0 ] 0)

    wrap identity (Element.element blockquote []) before == Ok after
    --> True

-}
wrap : (Block -> Block) -> Element -> Transform
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
                                        blockChildren (Array.map contentsMapFunc (Array.fromList [ bn ]))

                                    Inline il ->
                                        inlineChildren (Array.fromList [ il ])

                            newNode =
                                block elementParameters newChildren
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
                                                                block elementParameters
                                                                    (blockChildren <|
                                                                        Array.map
                                                                            contentsMapFunc
                                                                            (Array.slice childAnchorIndex
                                                                                (childFocusIndex + 1)
                                                                                (toBlockArray a)
                                                                            )
                                                                    )

                                                            newBlockArray =
                                                                blockChildren <|
                                                                    Array.append
                                                                        (Array.append
                                                                            (Array.Extra.sliceUntil
                                                                                childAnchorIndex
                                                                                (toBlockArray a)
                                                                            )
                                                                            (Array.fromList [ newChildNode ])
                                                                        )
                                                                        (Array.Extra.sliceFrom
                                                                            (childFocusIndex + 1)
                                                                            (toBlockArray a)
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


{-| Updates the state's selection to span the first and last element of the document.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "text" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "text" ])
                        ]
                )
            )
            (Just <| singleNodeRange [ 0, 0 ] 0 4)

    selectAll before == Ok after
    --> True

-}
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
                                            Text tl ->
                                                String.length (Text.text tl)

                                            InlineElement _ ->
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
                    |> withSelection (Just <| range first 0 last lastOffset)
                )


addLiftMarkToBlocksInSelection : Selection -> Block -> Block
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
                                Annotation.add Annotation.lift <| Block bn

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


{-| Lifts the selected block or the closest ancestor block out of its parent node.
If the current selection is a range selection, this function lifts all blocks that are in the range.
Returns an error if no lift can be done.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element blockquote [])
                            (blockChildren <|
                                Array.fromList
                                    [ block (Element.element paragraph [])
                                        (inlineChildren <| Array.fromList [ plainText "text" ])
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "text" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)

    lift before == Ok after
    --> True

-}
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
                    doLift markedRoot

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
                        (clear Annotation.lift <|
                            clearSelectionAnnotations liftedRoot
                        )
                )


{-| Same as `lift` but only succeeds if the selection is an empty text block.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element blockquote [])
                            (blockChildren <|
                                Array.fromList
                                    [ block (Element.element paragraph [])
                                        (inlineChildren <| Array.fromList [ plainText "" ])
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)

    liftEmpty before == Ok after
    --> True

-}
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
                            Err "I can only lift an empty text block"

                        else if List.length p < 2 then
                            Err "I cannot lift a node that's root or an immediate child of root"

                        else
                            lift editorState


{-| Does the split block logic, but also additionally changes the second part of the split
to the given element if it's an empty text block. This is useful for splitting a
header to a paragraph block.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element heading [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element heading [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        , block (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "" ])
                        ]
                )
            )
            (Just <| caret [ 1, 0 ] 0)

    splitBlockHeaderToNewParagraph before == after
    --> True

-}
splitBlockHeaderToNewParagraph : List String -> Element -> Transform
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
                                                Node.element bn
                                        in
                                        if
                                            List.member
                                                (Element.name parameters)
                                                headerElements
                                                && isEmptyTextBlock node
                                        then
                                            case
                                                replace p
                                                    (Block
                                                        (bn
                                                            |> withElement
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


{-| Transform that inserts the given block at the selection. This is useful for inserting things like
horizontal rules or other block leaf elements.

If the selection is a range selection, the contents of that selection are first removed,
then the insert command happens. If the inserted block is selectable, then the resulting selection will
be the block, otherwise it will be the next selectable block or inline.
Returns an error if the block could not be inserted.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "test" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 2)


    horizontalRuleBlock : Block
    horizontalRuleBlock =
        block
            (Element.element horizontalRule [])
            Leaf


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "te" ])
                        , horizontalRuleBlock
                        , block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "st" ])
                        ]
                )
            )
            (Just <| caret [ 1 ] 0)

    insertBlock horizontalRuleBlock before == Ok after
    --> True

-}
insertBlock : Block -> Transform
insertBlock node editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRange editorState |> Result.andThen (insertBlock node)

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
                                        (BlockFragment (Array.fromList [ bn, node ]))
                                        (State.root editorState)
                                of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelection =
                                                if isSelectable (Block node) then
                                                    caret (increment (anchorNode selection)) 0

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
                                        insertBlockBeforeSelection node splitEditorState


insertBlockBeforeSelection : Block -> Transform
insertBlockBeforeSelection node editorState =
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
                                        (BlockFragment (Array.fromList newFragment))
                                        markedRoot
                                of
                                    Err s ->
                                        Err s

                                    Ok newRoot ->
                                        let
                                            newSelection =
                                                if isSelectable (Block node) then
                                                    Just <| caret closestBlockPath 0

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


{-| Removes the previous inline element if the selection is an inline element or text with offset 0.
Returns an error if it was unable to remove the element.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    , inlineElement (Element.element image []) []
                                    , plainText "text2"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 2 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    , plainText "text2"
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 1 ] 0)

    backspaceInlineElement before == Ok after
    --> True

-}
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
                                    InlineElement _ ->
                                        case
                                            replaceWithFragment
                                                decrementedPath
                                                (InlineFragment Array.empty)
                                                (State.root editorState)
                                        of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                Ok
                                                    (editorState
                                                        |> withSelection (Just <| caret decrementedPath 0)
                                                        |> withRoot newRoot
                                                    )

                                    Text _ ->
                                        Err "There is no previous inline leaf element, found a text leaf"

                            Block _ ->
                                Err "There is no previous inline leaf element, found a block node"


{-| Removes the previous block leaf if the selection is at the beginning of a text block, otherwise
returns an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "p1" ])
                        , block
                            (Element.element horizontalRule [])
                            Leaf
                        , block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "p2" ])
                        ]
                )
            )
            (Just <| caret [ 2, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "p1" ])
                        , block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "p2" ])
                        ]
                )
            )
            (Just <| caret [ 1, 0 ] 0)

    backspaceBlock before == Ok after
    --> True

-}
backspaceBlock : Transform
backspaceBlock editorState =
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
                                        case replaceWithFragment path (BlockFragment Array.empty) markedRoot of
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


groupSameTypeInlineLeaf : Inline -> Inline -> Bool
groupSameTypeInlineLeaf a b =
    case a of
        InlineElement _ ->
            case b of
                InlineElement _ ->
                    True

                Text _ ->
                    False

        Text _ ->
            case b of
                Text _ ->
                    True

                InlineElement _ ->
                    False


textFromGroup : List Inline -> String
textFromGroup leaves =
    String.join "" <|
        List.map
            (\leaf ->
                case leaf of
                    Text t ->
                        Text.text t

                    _ ->
                        ""
            )
            leaves


lengthsFromGroup : List Inline -> List Int
lengthsFromGroup leaves =
    List.map
        (\il ->
            case il of
                Text tl ->
                    String.length (Text.text tl)

                InlineElement _ ->
                    0
        )
        leaves


{-| Removes the word before the collapsed selection, otherwise returns an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "this is an ex"
                                    , markedText "ample okay" [ mark bold [] ]
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 1 ] 6)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "this is an "
                                    , markedText "okay" [ mark bold [] ]
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 11)

    backspaceWord before == Ok after
    --> True

-}
backspaceWord : Transform
backspaceWord editorState =
    -- Overview:
    -- Find the inline fragment that represents connected text nodes
    -- get the text in that fragment
    -- translate the offset for that text
    -- find where to backspace
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
                                            (Array.toList (toInlineArray arr))
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
                                                    range
                                                        (p ++ [ newIndex ])
                                                        newOffset
                                                        (anchorNode selection)
                                                        (anchorOffset selection)

                                                newState =
                                                    editorState |> withSelection (Just newSelection)
                                            in
                                            removeRange newState

                            _ ->
                                Err "I expected an inline leaf array"


{-| Delete (forward) transform for a single character. This function has a few quirks in order to take
advantage of native delete behavior, namely:

  - selection offset = end of text leaf, try to delete the next text node's text
  - selection offset = 1 - end of text leaf, remove the last character (afterwards, the reduce behavior of `apply`
    may remove the text node)
  - any other offset, return an error to allow browser to do the default behavior

```
before : State
before =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "text"
                                , markedText "text2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 4)


after : State
after =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "text"
                                , markedText "ext2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)

deleteText before == Ok after
--> True
```

-}
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
                                    InlineElement _ ->
                                        Err "I cannot delete text if the selection an inline leaf"

                                    Text tl ->
                                        let
                                            textLength =
                                                String.length (Text.text tl)
                                        in
                                        if anchorOffset selection < (textLength - 1) then
                                            Err "I use the default behavior when deleting text when the anchor offset is not at the end of a text node"

                                        else if anchorOffset selection == (textLength - 1) then
                                            case
                                                replace
                                                    (anchorNode selection)
                                                    (Inline
                                                        (Text
                                                            (tl |> Text.withText (String.dropRight 1 (Text.text tl)))
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
                                                                Text _ ->
                                                                    let
                                                                        newSelection =
                                                                            singleNodeRange nextPath 0 1
                                                                    in
                                                                    removeRange
                                                                        (editorState
                                                                            |> withSelection (Just newSelection)
                                                                        )

                                                                InlineElement _ ->
                                                                    Err "Cannot delete if the previous node is an inline leaf"


{-| Removes the next inline element if the selection is at the end of a text leaf or inline element.
Returns an error if it was unable to remove the element.
-}
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
                                            Text t ->
                                                String.length (Text.text t)

                                            InlineElement _ ->
                                                0
                                in
                                if anchorOffset selection < length then
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
                                                        InlineElement _ ->
                                                            case
                                                                replaceWithFragment
                                                                    incrementedPath
                                                                    (InlineFragment Array.empty)
                                                                    (State.root editorState)
                                                            of
                                                                Err s ->
                                                                    Err s

                                                                Ok newRoot ->
                                                                    Ok (editorState |> withRoot newRoot)

                                                        Text _ ->
                                                            Err "There is no next inline leaf element, found a text leaf"

                                                Block _ ->
                                                    Err "There is no next inline leaf, found a block node"


{-| Removes the next block leaf if the selection is at the end of a text block, otherwise fails
with an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "p1" ])
                        , block
                            (Element.element horizontalRule [])
                            Leaf
                        , block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "p2" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 2)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "p1" ])
                        , block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "p2" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 2)

    deleteBlock before == Ok after
    --> True

-}
deleteBlock : Transform
deleteBlock editorState =
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
                                                (BlockFragment Array.empty)
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


{-| Removes the word after the collapsed selection, otherwise returns an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "this is an ex"
                                    , markedText "ample okay" [ mark bold [] ]
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 11)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "this is an "
                                    , markedText " okay" [ mark bold [] ]
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 11)

    deleteWord before == Ok after
    --> True

-}
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
                                            (Array.toList (toInlineArray arr))
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
                                                    range
                                                        (p ++ [ newIndex ])
                                                        newOffset
                                                        (anchorNode selection)
                                                        (anchorOffset selection)

                                                newState =
                                                    editorState |> withSelection (Just newSelection)
                                            in
                                            removeRange newState

                            _ ->
                                Err "I expected an inline leaf array"


isBlockLeaf : Selection -> Block -> Bool
isBlockLeaf selection root =
    case nodeAt (anchorNode selection) root of
        Nothing ->
            False

        Just n ->
            case n of
                Block b ->
                    case childNodes b of
                        Leaf ->
                            True

                        _ ->
                            False

                _ ->
                    False


firstSelectablePath : Block -> Maybe Path
firstSelectablePath block =
    case findForwardFromExclusive (\_ n -> isSelectable n) [] block of
        Nothing ->
            Nothing

        Just ( p, _ ) ->
            Just p


{-| Inserts the block after a block leaf.

    emptyParagraph : Block
    emptyParagraph =
        block
            (Element.element paragraph [])
            (inlineChildren <| Array.fromList [ plainText "" ])


    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "test" ])
                        , block
                            (Element.element horizontalRule [])
                            Leaf
                        ]
                )
            )
            (Just <| caret [ 1 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "test" ])
                        , block
                            (Element.element horizontalRule [])
                            Leaf
                        , emptyParagraph
                        ]
                )
            )
            (Just <| caret [ 2, 0 ] 0)

    insertAfterBlockLeaf emptyParagraph before == Ok after
    --> True

-}
insertAfterBlockLeaf : Block -> Transform
insertAfterBlockLeaf blockToInsert state =
    case State.selection state of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I cannot insert an empty paragraph unless the selection is collapsed"

            else if not <| isBlockLeaf selection (State.root state) then
                Err "I can only insert an element after a block leaf"

            else
                case insertAfter (anchorNode selection) (BlockFragment <| Array.fromList [ blockToInsert ]) (State.root state) of
                    Err s ->
                        Err s

                    Ok newRoot ->
                        let
                            relativeSelectablePath =
                                Maybe.withDefault [] (firstSelectablePath blockToInsert)

                            newAnchorPath =
                                increment (anchorNode selection) ++ relativeSelectablePath
                        in
                        Ok (State.state newRoot (Just <| caret newAnchorPath 0))


{-| Insert a newline at the selection in elements with the name whitelisted by the String list. This
is used by the code block element, since it only allows text (no line breaks or marks).
This is a somewhat specialized method, but may be useful outside of its narrow context.
-}
insertNewline : List String -> Transform
insertNewline elements editorState =
    let
        removedRangeEditorState =
            Result.withDefault editorState (removeRange editorState)
    in
    case State.selection removedRangeEditorState of
        Nothing ->
            Err "Invalid selection"

        Just selection ->
            if not <| isCollapsed selection then
                Err "I can only try to insert a newline if the selection is collapsed"

            else
                case findTextBlockNodeAncestor (anchorNode selection) (State.root removedRangeEditorState) of
                    Nothing ->
                        Err "No textblock node ancestor found"

                    Just ( _, textblock ) ->
                        if List.member (Element.name (Node.element textblock)) elements then
                            insertText "\n" removedRangeEditorState

                        else
                            Err "Selection is not a textblock"


{-| If the selection is collapsed at the beginning of a text block, this will select the previous
selectable node, and change the offset to the end if it's a text node. This is useful for default
backspace behavior in case a join backward operation could not be made.
-}
selectBackward : Transform
selectBackward state =
    case State.selection state of
        Nothing ->
            Err "There is no selection to move forward"

        Just selection ->
            let
                root =
                    State.root state
            in
            if not <| selectionIsBeginningOfTextBlock selection (State.root state) then
                Err "I can only select a node backwards if this is the beginning of a text block"

            else
                case findBackwardFromExclusive (\_ n -> isSelectable n) (anchorNode selection) root of
                    Nothing ->
                        Err "I could not find a selectable node prior to the selected one"

                    Just ( newAnchor, n ) ->
                        let
                            offset =
                                case n of
                                    Inline i ->
                                        case i of
                                            Text t ->
                                                String.length (Text.text t)

                                            _ ->
                                                0

                                    _ ->
                                        0
                        in
                        Ok (state |> withSelection (Just <| caret newAnchor offset))


{-| If the selection is collapsed at the end of a text block, this will select the next
selectable node at offset 0. This is useful for default delete behavior in case a
join forward operation could not be made.
-}
selectForward : Transform
selectForward state =
    case State.selection state of
        Nothing ->
            Err "There is no selection to move forward"

        Just selection ->
            let
                root =
                    State.root state
            in
            if not <| selectionIsEndOfTextBlock selection (State.root state) then
                Err "I can only select a node forward if this is the end of a text block"

            else
                case findForwardFromExclusive (\_ n -> isSelectable n) (anchorNode selection) root of
                    Nothing ->
                        Err "I could not find a selectable node after the selected one"

                    Just ( newAnchor, _ ) ->
                        Ok (state |> withSelection (Just <| caret newAnchor 0))
