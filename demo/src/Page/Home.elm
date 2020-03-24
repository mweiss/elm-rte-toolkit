module Page.Home exposing (..)

import Array
import Editor
import Html exposing (Html, h1, text)
import Html.Attributes exposing (class)
import RichTextEditor.Model.Element exposing (element)
import RichTextEditor.Model.Mark exposing (mark)
import RichTextEditor.Model.Node
    exposing
        ( Block
        , Inline(..)
        , block
        , blockChildren
        , inlineChildren
        , plainText
        )
import RichTextEditor.Model.State as State exposing (State)
import RichTextEditor.Model.Text as Text
import RichTextEditor.Specs as Specs exposing (code, doc, paragraph)
import Session exposing (Session)
import Set


type alias Model =
    { session : Session
    , editor : Editor.Model
    }


type Msg
    = EditorMsg Editor.EditorMsg
    | GotSession Session


dummyView : { title : String, content : List (Html msg) }
dummyView =
    { title = "Dummy Home", content = [ text "Dummy Home" ] }


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Home"
    , content =
        [ h1 [ class "main-header" ]
            [ text "Elm package for building rich text editors" ]
        , Html.map EditorMsg (Editor.view Editor.decorations Editor.commandBindings Specs.markdown model.editor)
        ]
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, editor = Editor.init initialState }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( e, _ ) =
                    Editor.update Editor.commandBindings Specs.markdown editorMsg model.editor
            in
            ( { model | editor = e }, Cmd.none )

        _ ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



----


initNode : Block
initNode =
    block
        (element doc [] Set.empty)
        (blockChildren (Array.fromList [ initialEditorNode ]))


initialEditorNode : Block
initialEditorNode =
    block
        (element paragraph [] Set.empty)
        (inlineChildren
            (Array.fromList
                [ plainText <|
                    "Rich Text Editor Toolkit is an open source project to make cross platform editors on the web. "
                        ++ "This package treats "
                , Text
                    (Text.empty
                        |> Text.withText "contenteditable"
                        |> Text.withMarks [ mark code [] ]
                    )
                , plainText <|
                    " as an I/O device, and uses browser events and mutation observers "
                        ++ "to detect changes and update itself.  The editor's model itself is defined "
                        ++ "and validated by a programmable specification that allows you to create a "
                        ++ "custom tailored editor that fits your needs."
                ]
            )
        )


initialState : State
initialState =
    State.state initNode Nothing
