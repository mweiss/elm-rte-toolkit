module RichTextEditor.Model.Editor exposing
    ( DecoderFunc
    , Editor
    , InternalEditorMsg(..)
    , bufferedEditorState
    , commandMap
    , completeRerenderCount
    , editor
    , forceCompleteRerender
    , forceRerender
    , forceReselection
    , history
    , isComposing
    , renderCount
    , selectionCount
    , spec
    , state
    , withBufferedEditorState
    , withCommandMap
    , withComposing
    , withHistory
    , withState
    )

{-| The decoder function is used to translate the messages the editor needs to your overall application.
A typical use case might be:

    type alias Model = { editor : Editor }

    type MyApplicationMsg
        = EditorInternalMsg InternalEditorMsg
        | OtherAppAction1
        | OtherAppAction2

    decoder =
        EditorInternalMsg

    update :
        update : MyApplicationMsg -> Model -> ( Model, Cmd MyApplicationMsg )
        update msg model =
            case msg of
                InternalMsg internalEditorMsg ->
                    ( { model | editor = Editor.internalUpdate internalEditorMsg model.editor }, Cmd.none )
                ...

-}

import RichTextEditor.Model.Command exposing (CommandMap, emptyCommandMap)
import RichTextEditor.Model.Event exposing (EditorChange, InputEvent, KeyboardEvent, PasteEvent)
import RichTextEditor.Model.History exposing (History, emptyHistory)
import RichTextEditor.Model.Selection exposing (Selection)
import RichTextEditor.Model.Spec exposing (Spec)
import RichTextEditor.Model.State exposing (State)


type alias DecoderFunc msg =
    InternalEditorMsg -> msg


{-| The internal events that an editor has to respond to. These events should be mapped via a DecoderFunc.
-}
type InternalEditorMsg
    = SelectionEvent (Maybe Selection) Bool
    | ChangeEvent EditorChange
    | BeforeInputEvent InputEvent
    | KeyDownEvent KeyboardEvent
    | CompositionStart
    | CompositionEnd
    | PasteWithDataEvent PasteEvent
    | CutEvent


type Editor
    = Editor EditorContents


{-| Represents a rich text editor. The state of the editor, along with render information,
decoder function, and command map.
-}
type alias EditorContents =
    { state : State
    , renderCount : Int
    , selectionCount : Int
    , completeRerenderCount : Int
    , isComposing : Bool
    , bufferedEditorState : Maybe State
    , commandMap : CommandMap
    , spec : Spec
    , history : History
    }


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


commandMap : Editor -> CommandMap
commandMap e =
    case e of
        Editor c ->
            c.commandMap


spec : Editor -> Spec
spec e =
    case e of
        Editor c ->
            c.spec


history : Editor -> History
history e =
    case e of
        Editor c ->
            c.history


withHistory : History -> Editor -> Editor
withHistory h e =
    case e of
        Editor c ->
            Editor { c | history = h }


withCommandMap : CommandMap -> Editor -> Editor
withCommandMap m e =
    case e of
        Editor c ->
            Editor { c | commandMap = m }


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


defaultDequeSize : Int
defaultDequeSize =
    64


editor : Spec -> State -> Editor
editor iSpec iState =
    Editor
        { renderCount = 0
        , bufferedEditorState = Nothing
        , completeRerenderCount = 0
        , selectionCount = 0
        , isComposing = False
        , commandMap = emptyCommandMap
        , spec = iSpec
        , state = iState
        , history = emptyHistory defaultDequeSize
        }
