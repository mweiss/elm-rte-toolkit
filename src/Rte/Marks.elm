module Rte.Marks exposing (..)

import Rte.Model exposing (Mark)


selectionMark : Mark
selectionMark =
    { name = "selection", attributes = [] }


selectableMark : Mark
selectableMark =
    { name = "selectable", attributes = [] }
