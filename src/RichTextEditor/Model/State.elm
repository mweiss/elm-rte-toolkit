module RichTextEditor.Model.State exposing (State, root, selection, withRoot, withSelection)

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


selection : State -> Maybe Selection
selection state =
    case state of
        State s ->
            s.selection


root : State -> EditorBlockNode
root state =
    case state of
        State s ->
            s.root


withSelection : Maybe Selection -> State -> State
withSelection sel state =
    case state of
        State s ->
            State { s | selection = sel }


withRoot : EditorBlockNode -> State -> State
withRoot node state =
    case state of
        State s ->
            State { s | root = node }
