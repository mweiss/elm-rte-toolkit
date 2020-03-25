module RichTextEditor.Model.History exposing (History, empty)

{-| This module contains the type used to store undo/redo history.

@docs History, empty

-}

import RichTextEditor.Internal.Model.History as Internal


{-| `History` contains the undo deque and redo stack related to undo history.
-}
type alias History =
    Internal.History


{-| Provides an empty `History` with the given config. The config values are as follows:

  - `groupDelayMilliseconds` is the interval which the editor will ignore adding multiple text changes onto the undo stack. This is
    so the history doesn't get overwhelmed by single character changes.
  - `size` is the number of states stored in the history

-}
empty : { groupDelayMilliseconds : Int, size : Int } -> History
empty =
    Internal.empty
