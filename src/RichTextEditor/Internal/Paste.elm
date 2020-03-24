module RichTextEditor.Internal.Paste exposing (..)

import Array exposing (Array)
import List.Extra
import Result exposing (Result)
import RichTextEditor.Commands
    exposing
        ( joinBackward
        , joinForward
        , removeRangeSelection
        , splitTextBlock
        )
import RichTextEditor.Internal.Constants exposing (zeroWidthSpace)
import RichTextEditor.Internal.Editor exposing (applyNamedCommandList)
import RichTextEditor.Internal.Spec exposing (htmlToElementArray)
import RichTextEditor.Model.Command exposing (Transform, transform)
import RichTextEditor.Model.Editor exposing (Editor)
import RichTextEditor.Model.Event exposing (PasteEvent)
import RichTextEditor.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , block
        , childNodes
        , element
        , inlineChildren
        , plainText
        , toInlineArray
        )
import RichTextEditor.Model.Selection
    exposing
        ( anchorNode
        , anchorOffset
        , caret
        , focusOffset
        , isCollapsed
        )
import RichTextEditor.Model.Spec exposing (Spec)
import RichTextEditor.Model.State as State exposing (withRoot, withSelection)
import RichTextEditor.Node
    exposing
        ( Fragment(..)
        , Node(..)
        , findTextBlockNodeAncestor
        , insertAfter
        , nodeAt
        , replaceWithFragment
        , splitTextLeaf
        )
import RichTextEditor.Path exposing (parent)
import RichTextEditor.Selection
    exposing
        ( annotateSelection
        , clearSelectionAnnotations
        , selectionFromAnnotations
        )


handlePaste : PasteEvent -> Spec -> Editor -> Editor
handlePaste event spec editor =
    let
        commandArray =
            [ ( "pasteHtml", transform <| pasteHtml spec event.html )
            , ( "pasteText", transform <| pasteText event.text )
            ]
    in
    Result.withDefault editor (applyNamedCommandList commandArray spec editor)


pasteText : String -> Transform
pasteText text editorState =
    if String.isEmpty text then
        Err "There is no text to paste"

    else
        case State.selection editorState of
            Nothing ->
                Err "Nothing is selected"

            Just selection ->
                if not <| isCollapsed selection then
                    removeRangeSelection editorState |> Result.andThen (pasteText text)

                else
                    let
                        lines =
                            String.split "\n" (String.replace zeroWidthSpace "" text)
                    in
                    case findTextBlockNodeAncestor (anchorNode selection) (State.root editorState) of
                        Nothing ->
                            Err "I can only paste test if there is a text block ancestor"

                        Just ( _, tbNode ) ->
                            let
                                newLines =
                                    List.map
                                        (\line ->
                                            block (element tbNode)
                                                (inlineChildren <|
                                                    Array.fromList
                                                        [ plainText line
                                                        ]
                                                )
                                        )
                                        lines

                                fragment =
                                    BlockNodeFragment (Array.fromList newLines)
                            in
                            pasteFragment fragment editorState


pasteHtml : Spec -> String -> Transform
pasteHtml spec html editorState =
    if String.isEmpty html then
        Err "There is no html to paste"

    else
        case htmlToElementArray spec html of
            Err s ->
                Err s

            Ok fragmentArray ->
                Array.foldl
                    (\fragment result ->
                        case result of
                            Err _ ->
                                result

                            Ok state ->
                                pasteFragment fragment state
                    )
                    (Ok editorState)
                    fragmentArray


pasteFragment : Fragment -> Transform
pasteFragment fragment editorState =
    case fragment of
        InlineLeafFragment a ->
            pasteInlineArray a editorState

        BlockNodeFragment a ->
            pasteBlockArray a editorState


pasteInlineArray : Array Inline -> Transform
pasteInlineArray inlineFragment editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (pasteInlineArray inlineFragment)

            else
                case findTextBlockNodeAncestor (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "I can only paste an inline array into a text block node"

                    Just ( path, node ) ->
                        case childNodes node of
                            BlockChildren _ ->
                                Err "I cannot add an inline array to a block array"

                            Leaf ->
                                Err "I cannot add an inline array to a block leaf"

                            InlineChildren a ->
                                case List.Extra.last (anchorNode selection) of
                                    Nothing ->
                                        Err "Invalid state, somehow the anchor node is the root node"

                                    Just index ->
                                        case Array.get index (toInlineArray a) of
                                            Nothing ->
                                                Err "Invalid anchor node path"

                                            Just inlineNode ->
                                                case inlineNode of
                                                    Text tl ->
                                                        let
                                                            ( previous, next ) =
                                                                splitTextLeaf (anchorOffset selection) tl

                                                            newFragment =
                                                                Array.fromList <| Text previous :: (Array.toList inlineFragment ++ [ Text next ])

                                                            replaceResult =
                                                                replaceWithFragment (anchorNode selection)
                                                                    (InlineLeafFragment newFragment)
                                                                    (State.root editorState)

                                                            newSelection =
                                                                caret (path ++ [ index + Array.length inlineFragment + 1 ]) 0
                                                        in
                                                        case replaceResult of
                                                            Err s ->
                                                                Err s

                                                            Ok newRoot ->
                                                                Ok
                                                                    (editorState
                                                                        |> withSelection (Just newSelection)
                                                                        |> withRoot newRoot
                                                                    )

                                                    InlineElement _ ->
                                                        let
                                                            replaceResult =
                                                                replaceWithFragment (anchorNode selection) (InlineLeafFragment inlineFragment) (State.root editorState)

                                                            newSelection =
                                                                caret (path ++ [ index + Array.length inlineFragment - 1 ]) 0
                                                        in
                                                        case replaceResult of
                                                            Err s ->
                                                                Err s

                                                            Ok newRoot ->
                                                                Ok
                                                                    (editorState
                                                                        |> withSelection (Just newSelection)
                                                                        |> withRoot newRoot
                                                                    )


pasteBlockArray : Array Block -> Transform
pasteBlockArray blockFragment editorState =
    -- split, add nodes, select beginning, join backwards, select end, join forward
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (pasteBlockArray blockFragment)

            else
                let
                    parentPath =
                        parent (anchorNode selection)
                in
                case nodeAt parentPath (State.root editorState) of
                    Nothing ->
                        Err "I cannot find the parent node of the selection"

                    Just parentNode ->
                        case parentNode of
                            Inline _ ->
                                Err "Invalid parent node"

                            Block bn ->
                                case childNodes bn of
                                    Leaf ->
                                        Err "Invalid parent node, somehow the parent node was a leaf"

                                    BlockChildren _ ->
                                        case
                                            replaceWithFragment (anchorNode selection)
                                                (BlockNodeFragment blockFragment)
                                                (State.root editorState)
                                        of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                case List.Extra.last (anchorNode selection) of
                                                    Nothing ->
                                                        Err "Invalid anchor node, somehow the parent is root"

                                                    Just index ->
                                                        let
                                                            newSelection =
                                                                caret (parentPath ++ [ index + Array.length blockFragment - 1 ]) 0
                                                        in
                                                        Ok
                                                            (editorState
                                                                |> withSelection (Just newSelection)
                                                                |> withRoot newRoot
                                                            )

                                    InlineChildren _ ->
                                        case splitTextBlock editorState of
                                            Err s ->
                                                Err s

                                            Ok splitEditorState ->
                                                case State.selection splitEditorState of
                                                    Nothing ->
                                                        Err "Invalid editor state selection after split action."

                                                    Just splitSelection ->
                                                        let
                                                            annotatedSelectionRoot =
                                                                annotateSelection splitSelection (State.root splitEditorState)
                                                        in
                                                        case insertAfter parentPath (BlockNodeFragment blockFragment) annotatedSelectionRoot of
                                                            Err s ->
                                                                Err s

                                                            Ok addedNodesRoot ->
                                                                let
                                                                    addNodesEditorState =
                                                                        editorState |> withRoot addedNodesRoot

                                                                    joinBeginningState =
                                                                        Result.withDefault
                                                                            addNodesEditorState
                                                                            (joinForward
                                                                                (addNodesEditorState
                                                                                    |> withSelection
                                                                                        (Just <|
                                                                                            caret
                                                                                                (anchorNode selection)
                                                                                                (anchorOffset selection)
                                                                                        )
                                                                                )
                                                                            )

                                                                    annotatedSelection =
                                                                        selectionFromAnnotations (State.root joinBeginningState)
                                                                            (anchorOffset splitSelection)
                                                                            (focusOffset splitSelection)

                                                                    joinEndState =
                                                                        Result.withDefault
                                                                            joinBeginningState
                                                                            (joinBackward (joinBeginningState |> withSelection annotatedSelection))
                                                                in
                                                                Ok (joinEndState |> withRoot (clearSelectionAnnotations (State.root joinEndState)))
