module RichTextEditor.Internal.Model.Selection exposing (..)

{-| A selection represents the information received and translated from the selection API. Note that
the anchorNode and focusNode are translations of the node paths relative to the editor.
-}

import RichTextEditor.Internal.Model.Node exposing (NodePath)


type Selection
    = Selection Contents


type alias Contents =
    { anchorOffset : Int
    , anchorNode : NodePath
    , focusOffset : Int
    , focusNode : NodePath
    }
