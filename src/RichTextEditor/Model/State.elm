module RichTextEditor.Model.State exposing
    ( State
    , isSame
    , root
    , selection
    , state
    , withRoot
    , withSelection
    )

{-| An EditorState is a tuple of an editor fragment and a selection. EditorState allows you to keep
track of and manipulate the contents of the editor.
-}

import RichTextEditor.Model.Node exposing (Block, isSameBlock)
import RichTextEditor.Model.Selection exposing (Selection)


type State
    = State Contents


type alias Contents =
    { root : Block
    , selection : Maybe Selection
    }


state : Block -> Maybe Selection -> State
state r s =
    State { root = r, selection = s }


selection : State -> Maybe Selection
selection st =
    case st of
        State s ->
            s.selection


root : State -> Block
root st =
    case st of
        State s ->
            s.root


withSelection : Maybe Selection -> State -> State
withSelection sel st =
    case st of
        State s ->
            State { s | selection = sel }


withRoot : Block -> State -> State
withRoot node st =
    case st of
        State s ->
            State { s | root = node }


isSame : State -> State -> Bool
isSame s1 s2 =
    isSameBlock (root s1) (root s2) && selection s1 == selection s2
