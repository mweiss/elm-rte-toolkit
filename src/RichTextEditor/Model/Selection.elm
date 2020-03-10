module RichTextEditor.Model.Selection exposing (..)

{-| A selection represents the information received and translated from the selection API. Note that
the anchorNode and focusNode are translations of the node paths relative to the editor.
-}

import RichTextEditor.Model.Node exposing (NodePath)


type Selection
    = Selection Contents


type alias Contents =
    { anchorOffset : Int
    , anchorNode : NodePath
    , focusOffset : Int
    , focusNode : NodePath
    }


anchorNode : Selection -> NodePath
anchorNode selection =
    case selection of
        Selection c ->
            c.anchorNode


anchorOffset : Selection -> Int
anchorOffset selection =
    case selection of
        Selection c ->
            c.anchorOffset


focusNode : Selection -> NodePath
focusNode selection =
    case selection of
        Selection c ->
            c.focusNode


focusOffset : Selection -> Int
focusOffset selection =
    case selection of
        Selection c ->
            c.focusOffset


{-| This is a helper method for constructing a caret selection.
-}
caretSelection : NodePath -> Int -> Selection
caretSelection nodePath offset =
    singleNodeRangeSelection nodePath offset offset


{-| This is a helper method for determining if a selection is collapsed.
-}
isCollapsed : Selection -> Bool
isCollapsed selection =
    case selection of
        Selection c ->
            c.anchorOffset == c.focusOffset && c.anchorNode == c.focusNode


{-| This is a helper method for creating a range selection
-}
rangeSelection : NodePath -> Int -> NodePath -> Int -> Selection
rangeSelection aNode aOffset fNode fOffset =
    Selection
        { anchorOffset = aOffset
        , anchorNode = aNode
        , focusOffset = fOffset
        , focusNode = fNode
        }


{-| This is a helper method for creating a selection over a single node
-}
singleNodeRangeSelection : NodePath -> Int -> Int -> Selection
singleNodeRangeSelection node aOffset fOffset =
    rangeSelection node aOffset node fOffset


{-| Sorts the selection's anchor to be before the focus. This method is helpful because in the selection
API, a selection's anchor node is not always before a selection's focus node, but when reasoning about editor
operations, we want the anchor to be before the focus.
-}
normalizeSelection : Selection -> Selection
normalizeSelection selection =
    case selection of
        Selection c ->
            Selection <|
                case compare c.anchorNode c.focusNode of
                    EQ ->
                        { c | anchorOffset = min c.focusOffset c.anchorOffset, focusOffset = max c.focusOffset c.anchorOffset }

                    LT ->
                        c

                    GT ->
                        { c | focusNode = c.anchorNode, focusOffset = c.anchorOffset, anchorNode = c.focusNode, anchorOffset = c.focusOffset }
