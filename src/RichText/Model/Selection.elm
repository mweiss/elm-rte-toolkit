module RichText.Model.Selection exposing
    ( Selection, anchorNode, anchorOffset, focusNode, focusOffset
    , caret, singleNodeRange, range
    , isCollapsed, normalize
    )

{-| A selection represents the information received and translated from the selection web API. Note that
the anchorNode and focusNode are translations of the node paths relative to the editor.


# Selection

@docs Selection, anchorNode, anchorOffset, focusNode, focusOffset


# Initialization

@docs caret, singleNodeRange, range


# Helpers

@docs isCollapsed, normalize

-}

import RichText.Model.Node exposing (Path)


{-| A `Selection` represents the information received and translated from the selection API. Note that
the anchorNode and focusNode are translations of the node paths relative to the editor.
-}
type Selection
    = Selection Contents


type alias Contents =
    { anchorOffset : Int
    , anchorNode : Path
    , focusOffset : Int
    , focusNode : Path
    }


{-| The path to the selection anchor node
-}
anchorNode : Selection -> Path
anchorNode selection =
    case selection of
        Selection c ->
            c.anchorNode


{-| The selection anchor offset
-}
anchorOffset : Selection -> Int
anchorOffset selection =
    case selection of
        Selection c ->
            c.anchorOffset


{-| The path to the selection focus node
-}
focusNode : Selection -> Path
focusNode selection =
    case selection of
        Selection c ->
            c.focusNode


{-| The selection focus offset
-}
focusOffset : Selection -> Int
focusOffset selection =
    case selection of
        Selection c ->
            c.focusOffset


{-| This is a helper method for constructing a caret selection.

    caret [0, 1] 0
    --> Creates a selection with { anchorNode=[0,1], anchorOffset=0, focusNode=[0,1], focusOffset=0 }

-}
caret : Path -> Int -> Selection
caret nodePath offset =
    singleNodeRange nodePath offset offset


{-| This is a helper method for creating a range selection

    range [0, 1] 0 [1, 1] 1
    --> Creates a selection with { anchorNode=[0,1], anchorOffset=0, focusNode=[1,1], focusOffset=1 }

-}
range : Path -> Int -> Path -> Int -> Selection
range aNode aOffset fNode fOffset =
    Selection
        { anchorOffset = aOffset
        , anchorNode = aNode
        , focusOffset = fOffset
        , focusNode = fNode
        }


{-| This is a helper method for creating a selection over a single node

    singleNodeRange [0, 1] 0 1
    --> Creates a selection with { anchorNode=[0,1], anchorOffset=0, focusNode=[0,1], focusOffset=1 }

-}
singleNodeRange : Path -> Int -> Int -> Selection
singleNodeRange node aOffset fOffset =
    range node aOffset node fOffset


{-| This is a helper method for determining if a selection is collapsed.

    isCollapsed <| singleNodeRange [0, 1] 0 1
    --> False

    isCollapsed <| caret [0, 1] 0
    --> True

-}
isCollapsed : Selection -> Bool
isCollapsed selection =
    case selection of
        Selection c ->
            c.anchorOffset == c.focusOffset && c.anchorNode == c.focusNode


{-| Sorts the selection's anchor to be before the focus. This method is helpful because in the selection
API, a selection's anchor node is not always before a selection's focus node, but when reasoning about editor
operations, we want the anchor to be before the focus.

    normalize <| range [ 1, 1 ] 0 [ 0, 1 ] 1
    --> { anchorNode=[0,1], anchorOffset=1, focusNode=[1,1], focusOffset=0 }

    normalize <| singleNodeRange [0, 1] 1 0
    --> { anchorNode=[0,1], anchorOffset=0, focusNode=[0,1], focusOffset=1 }

-}
normalize : Selection -> Selection
normalize selection =
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
