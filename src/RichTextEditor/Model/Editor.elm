module RichTextEditor.Model.Editor exposing
    ( Editor, editor, state, history, withState, withHistory
    , Message(..), Tagger
    , bufferedEditorState, withBufferedEditorState, shortKey, withShortKey, isComposing, withComposing
    , completeRerenderCount, forceCompleteRerender, forceRerender, forceReselection, renderCount, selectionCount
    )

{-| This module contains the types used to model the editor, as well as the messages used to
update the editor's internal state.


# Editor

@docs Editor, editor, state, history, withState, withHistory


# Messages

@docs Message, Tagger


# Render attributes

selectionCount, renderCount, completeRerenderCount, forceReselection, forceRerender, forceCompleteRerender


# Internal editor attributes

@docs bufferedEditorState, withBufferedEditorState, shortKey, withShortKey, isComposing, withComposing

-}

import RichTextEditor.Model.Event exposing (EditorChange, InitEvent, InputEvent, KeyboardEvent, PasteEvent)
import RichTextEditor.Model.History exposing (History, emptyHistory)
import RichTextEditor.Model.Keys exposing (meta)
import RichTextEditor.Model.Selection exposing (Selection)
import RichTextEditor.Model.State exposing (State)


type alias Tagger msg =
    Message -> msg


type Editor
    = Editor EditorContents


{-| Represents a rich text editor. The state of the editor, along with render information,
tagger function, and command map.
-}
type alias EditorContents =
    { state : State
    , renderCount : Int
    , selectionCount : Int
    , shortKey : String
    , completeRerenderCount : Int
    , isComposing : Bool
    , bufferedEditorState : Maybe State
    , history : History
    }


defaultDequeSize : Int
defaultDequeSize =
    64


editor : State -> Editor
editor iState =
    Editor
        { renderCount = 0
        , bufferedEditorState = Nothing
        , completeRerenderCount = 0
        , selectionCount = 0
        , shortKey = meta
        , isComposing = False
        , state = iState
        , history = emptyHistory defaultDequeSize
        }


{-| The internal events that an editor has to respond to. These events should be mapped via a Tagger.
-}
type Message
    = SelectionEvent (Maybe Selection) Bool
    | ChangeEvent EditorChange
    | BeforeInputEvent InputEvent
    | KeyDownEvent KeyboardEvent
    | CompositionStart
    | CompositionEnd
    | PasteWithDataEvent PasteEvent
    | CutEvent
    | ReplaceWith Editor
    | Init InitEvent


completeRerenderCount : Editor -> Int
completeRerenderCount e =
    case e of
        Editor c ->
            c.completeRerenderCount


selectionCount : Editor -> Int
selectionCount e =
    case e of
        Editor c ->
            c.selectionCount


renderCount : Editor -> Int
renderCount e =
    case e of
        Editor c ->
            c.renderCount


bufferedEditorState : Editor -> Maybe State
bufferedEditorState e =
    case e of
        Editor c ->
            c.bufferedEditorState


isComposing : Editor -> Bool
isComposing e =
    case e of
        Editor c ->
            c.isComposing


withComposing : Bool -> Editor -> Editor
withComposing composing e =
    case e of
        Editor c ->
            Editor { c | isComposing = composing }


withBufferedEditorState : Maybe State -> Editor -> Editor
withBufferedEditorState s e =
    case e of
        Editor c ->
            Editor { c | bufferedEditorState = s }


state : Editor -> State
state e =
    case e of
        Editor c ->
            c.state


withState : State -> Editor -> Editor
withState s e =
    case e of
        Editor c ->
            Editor { c | state = s }


history : Editor -> History
history e =
    case e of
        Editor c ->
            c.history


shortKey : Editor -> String
shortKey e =
    case e of
        Editor c ->
            c.shortKey


withHistory : History -> Editor -> Editor
withHistory h e =
    case e of
        Editor c ->
            Editor { c | history = h }


withShortKey : String -> Editor -> Editor
withShortKey key e =
    case e of
        Editor c ->
            Editor { c | shortKey = key }


forceRerender : Editor -> Editor
forceRerender e =
    case e of
        Editor c ->
            Editor { c | renderCount = c.renderCount + 1 }


forceReselection : Editor -> Editor
forceReselection e =
    case e of
        Editor c ->
            Editor { c | selectionCount = c.selectionCount + 1 }


forceCompleteRerender : Editor -> Editor
forceCompleteRerender e =
    case e of
        Editor c ->
            Editor { c | completeRerenderCount = c.completeRerenderCount + 1 }
