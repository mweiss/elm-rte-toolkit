module Rte.Selection exposing (..)

import Rte.Model exposing (EditorBlockNode, Selection, Spec)


domToEditor : Spec -> EditorBlockNode -> Selection -> Maybe Selection
domToEditor spec node selection =
    Nothing


editorToDom : Spec -> EditorBlockNode -> Selection -> Selection
editorToDom spec node selection =
    selection
