module Rte.Marks exposing (..)

import Rte.Model exposing (EditorInlineLeaf(..), Mark)


selectionMark : Mark
selectionMark =
    { name = "selection", attributes = [] }


selectableMark : Mark
selectableMark =
    { name = "selectable", attributes = [] }


findMarksFromInlineLeaf : EditorInlineLeaf -> List Mark
findMarksFromInlineLeaf leaf =
    case leaf of
        TextLeaf l ->
            l.marks

        InlineLeaf l ->
            l.marks
