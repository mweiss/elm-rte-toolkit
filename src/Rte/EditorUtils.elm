module Rte.EditorUtils exposing (..)

import Rte.Model exposing (Editor)


zeroWidthSpace =
    "\u{200B}"


forceRerender : Editor msg -> Editor msg
forceRerender editor =
    { editor | renderCount = editor.renderCount + 1 }
