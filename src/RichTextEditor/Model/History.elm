module RichTextEditor.Model.History exposing (..)

import BoundedDeque exposing (BoundedDeque)
import RichTextEditor.Model.State exposing (State)


type History
    = History Contents


type alias Contents =
    { undoDeque : BoundedDeque ( String, State )
    , redoStack : List State
    }


contents : History -> Contents
contents history =
    case history of
        History c ->
            c


fromContents : Contents -> History
fromContents c =
    History c
