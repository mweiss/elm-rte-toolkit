module RichTextEditor.Model.History exposing (History, emptyHistory, Contents, contents, fromContents)

{-| This module contains the data structure for undo/redo history.

@docs History, emptyHistory, Contents, contents, fromContents

-}

import BoundedDeque exposing (BoundedDeque)
import RichTextEditor.Model.State exposing (State)


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


{-| Initializes a `History` with an empty Deque and initial size
-}
emptyHistory : Int -> History
emptyHistory size =
    History
        { undoDeque = BoundedDeque.empty size
        , redoStack = []
        , groupDelayMilliseconds = 500
        , lastTextChangeTimestamp = 0
        }
