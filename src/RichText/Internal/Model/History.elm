module RichText.Internal.Model.History exposing (..)

{-| This module contains the internal data structure for undo/redo history.
-}

import BoundedDeque exposing (BoundedDeque)
import RichText.Model.State exposing (State)


{-| `History` contains the undo deque and redo stack related to undo history.
-}
type History
    = History Contents


{-| The contents used to initialize history, namely:

  - `undoDeque` is a deque of (action-name, previousStack)
  - `redoStack` is a list of states that have just been undone
  - `groupDelayMilliseconds` is the delay which the editor will group text changes into one entry
  - `lastTextChangeTimestamp` is the last text change timestamp in milliseconds

-}
type alias Contents =
    { undoDeque : BoundedDeque ( String, State )
    , redoStack : List State
    , groupDelayMilliseconds : Int
    , lastTextChangeTimestamp : Int
    }


{-| Retrieves the contents of `History`
-}
contents : History -> Contents
contents history =
    case history of
        History c ->
            c


{-| Initializes history from `Contents`
-}
fromContents : Contents -> History
fromContents c =
    History c


{-| Initializes an `History` with an empty Deque and initial size

  - `groupDelayMilliseconds` is the delay which the editor will group text changes into one entry
  - `size` is the number of states stored in the history

-}
empty : { groupDelayMilliseconds : Int, size : Int } -> History
empty config =
    History
        { undoDeque = BoundedDeque.empty config.size
        , redoStack = []
        , groupDelayMilliseconds = config.groupDelayMilliseconds
        , lastTextChangeTimestamp = 0
        }
