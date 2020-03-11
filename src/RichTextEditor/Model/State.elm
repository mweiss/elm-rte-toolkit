module RichTextEditor.Model.State exposing (State, root, selection, state, withRoot, withSelection)

{-| An EditorState is a tuple of an editor fragment and a selection. EditorState allows you to keep
track of and manipulate the contents of the editor.
-}

import RichTextEditor.Model.Node exposing (BlockNode)
import RichTextEditor.Model.Selection exposing (Selection)


type State
    = State Contents


type alias Contents =
    { root : BlockNode
    , selection : Maybe Selection
    }


state : BlockNode -> Maybe Selection -> State
state r s =
    State { root = r, selection = s }


selection : State -> Maybe Selection
selection st =
    case st of
        State s ->
            s.selection


root : State -> BlockNode
root st =
    case st of
        State s ->
            s.root


withSelection : Maybe Selection -> State -> State
withSelection sel st =
    case st of
        State s ->
            State { s | selection = sel }


withRoot : BlockNode -> State -> State
withRoot node st =
    case st of
        State s ->
            State { s | root = node }
