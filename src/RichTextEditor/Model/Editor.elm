module RichTextEditor.Model.Editor exposing
    ( DecoderFunc
    , Decorations
    , Editor
    , ElementDecoratorFunction
    , InternalEditorMsg(..)
    , MarkDecoratorFunction
    , bufferedEditorState
    , commandMap
    , completeRerenderCount
    , decoder
    , decorations
    , editor
    , elementDecorators
    , emptyDecorations
    , forceCompleteRerender
    , forceRerender
    , forceReselection
    , history
    , isComposing
    , markDecorators
    , renderCount
    , selectionCount
    , spec
    , state
    , withBufferedEditorState
    , withCommandMap
    , withComposing
    , withDecorations
    , withElementDecorators
    , withHistory
    , withMarkDecorators
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
import RichTextEditor.Model.Command exposing (CommandMap, emptyCommandMap)
import RichTextEditor.Model.Event exposing (EditorChange, InputEvent, KeyboardEvent, PasteEvent)
import RichTextEditor.Model.History exposing (History, emptyHistory, fromContents)
import RichTextEditor.Model.Mark exposing (Mark)
import RichTextEditor.Model.Node exposing (ElementParameters, Path)
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


decorations : Editor msg -> Decorations msg
decorations e =
    case e of
        Editor c ->
            c.decorations


completeRerenderCount : Editor msg -> Int
completeRerenderCount e =
    case e of
        Editor c ->
            c.completeRerenderCount


selectionCount : Editor msg -> Int
selectionCount e =
    case e of
        Editor c ->
            c.selectionCount


renderCount : Editor msg -> Int
renderCount e =
    case e of
        Editor c ->
            c.renderCount


bufferedEditorState : Editor msg -> Maybe State
bufferedEditorState e =
    case e of
        Editor c ->
            c.bufferedEditorState


isComposing : Editor msg -> Bool
isComposing e =
    case e of
        Editor c ->
            c.isComposing


withComposing : Bool -> Editor msg -> Editor msg
withComposing composing e =
    case e of
        Editor c ->
            Editor { c | isComposing = composing }


withBufferedEditorState : Maybe State -> Editor msg -> Editor msg
withBufferedEditorState s e =
    case e of
        Editor c ->
            Editor { c | bufferedEditorState = s }


state : Editor msg -> State
state e =
    case e of
        Editor c ->
            c.state


withState : State -> Editor msg -> Editor msg
withState s e =
    case e of
        Editor c ->
            Editor { c | state = s }


decoder : Editor msg -> DecoderFunc msg
decoder e =
    case e of
        Editor c ->
            c.decoder


commandMap : Editor msg -> CommandMap
commandMap e =
    case e of
        Editor c ->
            c.commandMap


spec : Editor msg -> Spec
spec e =
    case e of
        Editor c ->
            c.spec


history : Editor msg -> History
history e =
    case e of
        Editor c ->
            c.history


withHistory : History -> Editor msg -> Editor msg
withHistory h e =
    case e of
        Editor c ->
            Editor { c | history = h }


withDecorations : Decorations msg -> Editor msg -> Editor msg
withDecorations d e =
    case e of
        Editor c ->
            Editor { c | decorations = d }


withCommandMap : CommandMap -> Editor msg -> Editor msg
withCommandMap m e =
    case e of
        Editor c ->
            Editor { c | commandMap = m }


forceRerender : Editor msg -> Editor msg
forceRerender e =
    case e of
        Editor c ->
            Editor { c | renderCount = c.renderCount + 1 }


forceReselection : Editor msg -> Editor msg
forceReselection e =
    case e of
        Editor c ->
            Editor { c | selectionCount = c.selectionCount + 1 }


forceCompleteRerender : Editor msg -> Editor msg
forceCompleteRerender e =
    case e of
        Editor c ->
            Editor { c | completeRerenderCount = c.completeRerenderCount + 1 }


type Decorations msg
    = Decorations (DecorationsContents msg)


type alias DecorationsContents msg =
    { marks : Dict String (List (MarkDecoratorFunction msg))
    , elements : Dict String (List (ElementDecoratorFunction msg))
    }


emptyDecorations : Decorations msg
emptyDecorations =
    Decorations { marks = Dict.empty, elements = Dict.empty }


markDecorators : Decorations msg -> Dict String (List (MarkDecoratorFunction msg))
markDecorators d =
    case d of
        Decorations c ->
            c.marks


elementDecorators : Decorations msg -> Dict String (List (ElementDecoratorFunction msg))
elementDecorators d =
    case d of
        Decorations c ->
            c.elements


withMarkDecorators : Dict String (List (MarkDecoratorFunction msg)) -> Decorations msg -> Decorations msg
withMarkDecorators marks d =
    case d of
        Decorations c ->
            Decorations { c | marks = marks }


withElementDecorators : Dict String (List (ElementDecoratorFunction msg)) -> Decorations msg -> Decorations msg
withElementDecorators elements d =
    case d of
        Decorations c ->
            Decorations { c | elements = elements }


type alias ElementDecoratorFunction msg =
    DecoderFunc msg -> Path -> ElementParameters -> Path -> List (Html.Attribute msg)


type alias MarkDecoratorFunction msg =
    DecoderFunc msg -> Path -> Mark -> Path -> List (Html.Attribute msg)


defaultDequeSize : Int
defaultDequeSize =
    64


editor : Spec -> State -> DecoderFunc msg -> Editor msg
editor iSpec iState iDecoder =
    Editor
        { renderCount = 0
        , bufferedEditorState = Nothing
        , completeRerenderCount = 0
        , selectionCount = 0
        , isComposing = False
        , decoder = iDecoder
        , decorations = emptyDecorations
        , commandMap = emptyCommandMap
        , spec = iSpec
        , state = iState
        , history = emptyHistory defaultDequeSize
        }
