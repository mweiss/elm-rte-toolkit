module RichTextEditor.Internal.Model.EditorState exposing (State)

{-| An EditorState is a tuple of an editor fragment and a selection. EditorState allows you to keep
track of and manipulate the contents of the editor.
-}

import RichTextEditor.Internal.Model.Node exposing (EditorBlockNode)
import RichTextEditor.Internal.Model.Selection exposing (Selection)


type State
    = State Contents


type alias Contents =
    { root : EditorBlockNode
    , selection : Maybe Selection
    }
