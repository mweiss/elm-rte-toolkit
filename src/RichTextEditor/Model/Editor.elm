module RichTextEditor.Model.Editor exposing
    ( DecoderFunc
    , Decorations
    , Editor
    , ElementDecoratorFunction
    , InternalEditorMsg(..)
    , MarkDecoratorFunction
    , commandMap
    , decoder
    , elementDecorators
    , forceCompleteRerender
    , forceRerender
    , forceReselection
    , history
    , markDecorators
    , spec
    , state
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
import RichTextEditor.Model.Command exposing (CommandMap)
import RichTextEditor.Model.Event exposing (EditorChange, InputEvent, KeyboardEvent, PasteEvent)
import RichTextEditor.Model.History exposing (History)
import RichTextEditor.Model.Mark exposing (Mark)
import RichTextEditor.Model.Node exposing (ElementParameters, NodePath)
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


emptyDecorations : Decorations msg
emptyDecorations =
    Decorations { marks = Dict.empty, elements = Dict.empty }


markDecorators : Decorations msg -> Dict String (List (MarkDecoratorFunction msg))
markDecorators decorations =
    case decorations of
        Decorations c ->
            c.marks


elementDecorators : Decorations msg -> Dict String (List (ElementDecoratorFunction msg))
elementDecorators decorations =
    case decorations of
        Decorations c ->
            c.elements


withMarkDecorators : Dict String (List (MarkDecoratorFunction msg)) -> Decorations msg -> Decorations msg
withMarkDecorators marks decorations =
    case decorations of
        Decorations c ->
            Decorations { c | marks = marks }


withElementDecorators : Dict String (List (ElementDecoratorFunction msg)) -> Decorations msg -> Decorations msg
withElementDecorators elements decorations =
    case decorations of
        Decorations c ->
            Decorations { c | elements = elements }


type alias ElementDecoratorFunction msg =
    DecoderFunc msg -> NodePath -> ElementParameters -> NodePath -> List (Html.Attribute msg)


type alias MarkDecoratorFunction msg =
    DecoderFunc msg -> NodePath -> Mark -> NodePath -> List (Html.Attribute msg)
