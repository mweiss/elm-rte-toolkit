module RichTextEditor.Model.Editor exposing
    ( Editor, editor, state, history, shortKey, withHistory
    , Message
    )

{-| This module contains the types used to model the editor, as well as the messages used to
update the editor's internal state.


# Editor

@docs Editor, editor, state, history, shortKey, withHistory


# Messages

@docs Message

-}

import RichTextEditor.Internal.Model.Editor as InternalEditor
import RichTextEditor.Model.History exposing (History)
import RichTextEditor.Model.State exposing (State)


{-| `Editor` represents the entire state of the editor, and is what you store in your model.
-}
type alias Editor =
    InternalEditor.Editor


{-| Initializes an editor

    editor <| State.state docNode Nothing

-}
editor : State -> Editor
editor =
    InternalEditor.editor


{-| The internal events that an editor has to respond to.
-}
type alias Message =
    InternalEditor.Message


{-| Retrieves the current state from the editor
-}
state : Editor -> State
state =
    InternalEditor.state


{-| Retrieves the current history from the editor
-}
history : Editor -> History
history =
    InternalEditor.history


{-| Retrieves the shortKey from the editor. Note that this gets updated after the editor has been
rendered.
-}
shortKey : Editor -> String
shortKey =
    InternalEditor.shortKey


{-| Sets the history on the editor.

    editor
        |> withHistory newHistory

-}
withHistory : History -> Editor -> Editor
withHistory =
    InternalEditor.withHistory
