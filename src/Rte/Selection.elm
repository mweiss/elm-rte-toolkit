module Rte.Selection exposing (caretSelection, domToEditor, editorToDom, isCollapsed, rangeSelection, singleNodeRangeSelection)

import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf(..), ElementParameters, HtmlNode(..), Mark, NodePath, Selection, Spec)
import Rte.NodePath as Path


domToEditor : Spec -> EditorBlockNode -> Selection -> Maybe Selection
domToEditor =
    transformSelection Path.domToEditor


editorToDom : Spec -> EditorBlockNode -> Selection -> Maybe Selection
editorToDom =
    transformSelection Path.editorToDom


transformSelection : (Spec -> EditorBlockNode -> NodePath -> Maybe NodePath) -> Spec -> EditorBlockNode -> Selection -> Maybe Selection
transformSelection transformation spec node selection =
    case transformation spec node selection.anchorNode of
        Nothing ->
            Nothing

        Just anchorNode ->
            case transformation spec node selection.focusNode of
                Nothing ->
                    Nothing

                Just focusNode ->
                    Just <| rangeSelection anchorNode selection.anchorOffset focusNode selection.focusOffset


{-| This is a helper method for constructing a caret selection.
-}
caretSelection : NodePath -> Int -> Selection
caretSelection nodePath offset =
    singleNodeRangeSelection nodePath offset offset


{-| This is a helper method for determining if a selection is collapsed.
-}
isCollapsed : Selection -> Bool
isCollapsed selection =
    selection.anchorOffset == selection.focusOffset && selection.anchorNode == selection.focusNode


{-| This is a helper method for creating a range selection
-}
rangeSelection : NodePath -> Int -> NodePath -> Int -> Selection
rangeSelection anchorNode anchorOffset focusNode focusOffset =
    { anchorOffset = anchorOffset
    , anchorNode = anchorNode
    , focusOffset = focusOffset
    , focusNode = focusNode
    }


{-| This is a helper method for creating a selection over a single node
-}
singleNodeRangeSelection : NodePath -> Int -> Int -> Selection
singleNodeRangeSelection node anchorOffset focusOffset =
    rangeSelection node anchorOffset node focusOffset
