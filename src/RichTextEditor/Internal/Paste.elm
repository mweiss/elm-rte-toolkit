module RichTextEditor.Paste exposing (..)

import Array exposing (Array)
import List.Extra
import Result exposing (Result)
import RichTextEditor.Commands exposing (joinBackward, joinForward, removeRangeSelection, splitTextBlock)
import RichTextEditor.Editor exposing (applyNamedCommandList)
import RichTextEditor.Internal.Model
    exposing
        ( ChildNodes(..)
        , Editor
        , EditorBlockNode
        , EditorFragment(..)
        , EditorInlineLeaf(..)
        , EditorNode(..)
        , PasteEvent
        , Spec
        , Transform
        , inlineLeafArray
        , transformCommand
        , zeroWidthSpace
        )
import RichTextEditor.Node
    exposing
        ( findTextBlockNodeAncestor
        , insert
        , nodeAt
        , replaceWithFragment
        , splitTextLeaf
        )
import RichTextEditor.NodePath exposing (parent)
import RichTextEditor.Selection
    exposing
        ( annotateSelection
        , caretSelection
        , clearSelectionAnnotations
        , isCollapsed
        , selectionFromAnnotations
        )
import RichTextEditor.Spec exposing (htmlToElementArray)
import Set


handlePaste : PasteEvent -> Editor msg -> Editor msg
handlePaste event editor =
    let
        commandArray =
            [ ( "pasteHtml", transformCommand <| pasteHtml editor.spec event.html )
            , ( "pasteText", transformCommand <| pasteText event.text )
            ]
    in
    Result.withDefault editor (applyNamedCommandList commandArray editor)


pasteText : String -> Transform
pasteText text editorState =
    if String.isEmpty text then
        Err "There is no text to paste"

    else
        case editorState.selection of
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
                    case findTextBlockNodeAncestor selection.anchorNode editorState.root of
                        Nothing ->
                            Err "I can only paste test if there is a text block ancestor"

                        Just ( tbPath, tbNode ) ->
                            let
                                newLines =
                                    List.map
                                        (\line ->
                                            { parameters = tbNode.parameters
                                            , childNodes =
                                                inlineLeafArray <|
                                                    Array.fromList
                                                        [ TextLeaf
                                                            { text = line
                                                            , marks = []
                                                            , annotations = Set.empty
                                                            }
                                                        ]
                                            }
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


pasteFragment : EditorFragment -> Transform
pasteFragment fragment editorState =
    case fragment of
        InlineLeafFragment a ->
            pasteInlineArray a editorState

        BlockNodeFragment a ->
            pasteBlockArray a editorState


pasteInlineArray : Array EditorInlineLeaf -> Transform
pasteInlineArray inlineFragment editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (pasteInlineArray inlineFragment)

            else
                case findTextBlockNodeAncestor selection.anchorNode editorState.root of
                    Nothing ->
                        Err "I can only paste an inline array into a text block node"

                    Just ( path, node ) ->
                        case node.childNodes of
                            BlockArray _ ->
                                Err "I cannot add an inline array to a block array"

                            Leaf ->
                                Err "I cannot add an inline array to a block leaf"

                            InlineLeafArray a ->
                                case List.Extra.last selection.anchorNode of
                                    Nothing ->
                                        Err "Invalid state, somehow the anchor node is the root node"

                                    Just index ->
                                        case Array.get index a.array of
                                            Nothing ->
                                                Err "Invalid anchor node path"

                                            Just inlineNode ->
                                                case inlineNode of
                                                    TextLeaf tl ->
                                                        let
                                                            ( previous, next ) =
                                                                splitTextLeaf selection.anchorOffset tl

                                                            newFragment =
                                                                Array.fromList <| TextLeaf previous :: (Array.toList inlineFragment ++ [ TextLeaf next ])

                                                            replaceResult =
                                                                replaceWithFragment selection.anchorNode (InlineLeafFragment newFragment) editorState.root

                                                            newSelection =
                                                                caretSelection (path ++ [ index + Array.length inlineFragment + 1 ]) 0
                                                        in
                                                        case replaceResult of
                                                            Err s ->
                                                                Err s

                                                            Ok newRoot ->
                                                                Ok { editorState | selection = Just newSelection, root = newRoot }

                                                    InlineLeaf _ ->
                                                        let
                                                            replaceResult =
                                                                replaceWithFragment selection.anchorNode (InlineLeafFragment inlineFragment) editorState.root

                                                            newSelection =
                                                                caretSelection (path ++ [ index + Array.length inlineFragment - 1 ]) 0
                                                        in
                                                        case replaceResult of
                                                            Err s ->
                                                                Err s

                                                            Ok newRoot ->
                                                                Ok { editorState | selection = Just newSelection, root = newRoot }


pasteBlockArray : Array EditorBlockNode -> Transform
pasteBlockArray blockFragment editorState =
    -- split, add nodes, select beginning, join backwards, select end, join forward
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isCollapsed selection then
                removeRangeSelection editorState |> Result.andThen (pasteBlockArray blockFragment)

            else
                let
                    parentPath =
                        parent selection.anchorNode
                in
                case nodeAt parentPath editorState.root of
                    Nothing ->
                        Err "I cannot find the parent node of the selection"

                    Just parentNode ->
                        case parentNode of
                            InlineLeafWrapper _ ->
                                Err "Invalid parent node"

                            BlockNodeWrapper bn ->
                                case bn.childNodes of
                                    Leaf ->
                                        Err "Invalid parent node, somehow the parent node was a leaf"

                                    BlockArray _ ->
                                        case replaceWithFragment selection.anchorNode (BlockNodeFragment blockFragment) editorState.root of
                                            Err s ->
                                                Err s

                                            Ok newRoot ->
                                                case List.Extra.last selection.anchorNode of
                                                    Nothing ->
                                                        Err "Invalid anchor node, somehow the parent is root"

                                                    Just index ->
                                                        let
                                                            newSelection =
                                                                caretSelection (parentPath ++ [ index + Array.length blockFragment - 1 ]) 0
                                                        in
                                                        Ok { editorState | root = newRoot, selection = Just newSelection }

                                    InlineLeafArray a ->
                                        case splitTextBlock editorState of
                                            Err s ->
                                                Err s

                                            Ok splitEditorState ->
                                                case splitEditorState.selection of
                                                    Nothing ->
                                                        Err "Invalid editor state selection after split action."

                                                    Just splitSelection ->
                                                        let
                                                            annotatedSelectionRoot =
                                                                annotateSelection splitSelection splitEditorState.root
                                                        in
                                                        case insert parentPath (BlockNodeFragment blockFragment) annotatedSelectionRoot of
                                                            Err s ->
                                                                Err s

                                                            Ok addedNodesRoot ->
                                                                let
                                                                    addNodesEditorState =
                                                                        { editorState | root = addedNodesRoot }

                                                                    joinBeginningState =
                                                                        Result.withDefault
                                                                            addNodesEditorState
                                                                            (joinForward { addNodesEditorState | selection = Just <| caretSelection selection.anchorNode selection.anchorOffset })

                                                                    annotatedSelection =
                                                                        selectionFromAnnotations joinBeginningState.root splitSelection.anchorOffset splitSelection.focusOffset

                                                                    joinEndState =
                                                                        Result.withDefault
                                                                            joinBeginningState
                                                                            (joinBackward { joinBeginningState | selection = annotatedSelection })
                                                                in
                                                                Ok { joinEndState | root = clearSelectionAnnotations joinEndState.root }
