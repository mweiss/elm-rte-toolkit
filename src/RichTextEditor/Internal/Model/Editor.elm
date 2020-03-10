module RichTextEditor.Internal.Model.Editor exposing
    ( Decorations
    , Editor
    , InternalEditorMsg(..)
    , commandMap
    , decoder
    , history
    , state
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

import Dict exposing (Dict)
import Html
import RichTextEditor.Internal.Model.Command exposing (CommandMap)
import RichTextEditor.Internal.Model.EditorState exposing (State)
import RichTextEditor.Internal.Model.Event exposing (EditorChange, InputEvent, KeyboardEvent, PasteEvent)
import RichTextEditor.Internal.Model.History exposing (History)
import RichTextEditor.Internal.Model.Mark exposing (Mark)
import RichTextEditor.Internal.Model.Node exposing (ElementParameters, NodePath)
import RichTextEditor.Internal.Model.Selection exposing (Selection)
import RichTextEditor.Internal.Model.Spec exposing (Spec)


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


type Editor msg
    = Editor (EditorContents msg)


{-| Represents a rich text editor. The state of the editor, along with render information,
decoder function, and command map.
-}
type alias EditorContents msg =
    { state : State
    , renderCount : Int
    , selectionCount : Int
    , completeRerenderCount : Int
    , isComposing : Bool
    , decoder : DecoderFunc msg
    , bufferedEditorState : Maybe State
    , decorations : Decorations msg
    , commandMap : CommandMap
    , spec : Spec
    , history : History
    }


state : Editor msg -> State
state editor =
    case editor of
        Editor c ->
            c.state


withState : State -> Editor msg -> Editor msg
withState s editor =
    case editor of
        Editor c ->
            Editor { c | state = s }


decoder : Editor msg -> DecoderFunc msg
decoder editor =
    case editor of
        Editor c ->
            c.decoder


commandMap : Editor msg -> CommandMap
commandMap editor =
    case editor of
        Editor c ->
            c.commandMap


spec : Editor msg -> Spec
spec editor =
    case editor of
        Editor c ->
            c.spec


history : Editor msg -> History
history editor =
    case editor of
        Editor c ->
            c.history


withHistory : History -> Editor msg -> Editor msg
withHistory h editor =
    case editor of
        Editor c ->
            Editor { c | history = h }


forceRerender : Editor msg -> Editor msg
forceRerender editor =
    case editor of
        Editor c ->
            Editor { c | renderCount = c.renderCount + 1 }


forceReselection : Editor msg -> Editor msg
forceReselection editor =
    case editor of
        Editor c ->
            Editor { c | selectionCount = c.selectionCount + 1 }


forceCompleteRerender : Editor msg -> Editor msg
forceCompleteRerender editor =
    case editor of
        Editor c ->
            Editor { c | completeRerenderCount = c.completeRerenderCount + 1 }


type Decorations msg
    = Decorations (DecorationsContents msg)


type alias DecorationsContents msg =
    { marks : Dict String (List (MarkDecoratorFunction msg))
    , elements : Dict String (List (ElementDecoratorFunction msg))
    }


type alias ElementDecoratorFunction msg =
    DecoderFunc msg -> NodePath -> ElementParameters -> NodePath -> List (Html.Attribute msg)


type alias MarkDecoratorFunction msg =
    DecoderFunc msg -> NodePath -> Mark -> NodePath -> List (Html.Attribute msg)
