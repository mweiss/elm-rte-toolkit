module RichTextEditor.Model.State exposing (State, root, selection, state, withRoot, withSelection)

{-| An EditorState is a tuple of an editor fragment and a selection. EditorState allows you to keep
track of and manipulate the contents of the editor.
-}

import RichTextEditor.Model.Node exposing (EditorBlockNode)
import RichTextEditor.Model.Selection exposing (Selection)


type State
    = State Contents


type alias Contents =
    { root : EditorBlockNode
    , selection : Maybe Selection
    }


state : EditorBlockNode -> Maybe Selection -> State
state r s =
    State { root = r, selection = s }


selection : State -> Maybe Selection
selection st =
    case st of
        State s ->
            s.selection


root : State -> EditorBlockNode
root st =
    case st of
        State s ->
            s.root


withSelection : Maybe Selection -> State -> State
withSelection sel st =
    case st of
        State s ->
            State { s | selection = sel }


withRoot : EditorBlockNode -> State -> State
withRoot node st =
    case st of
        State s ->
            State { s | root = node }
