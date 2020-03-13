module Page.Home exposing (..)

import FullEditor
import Html exposing (Html, h1, text)
import RichTextEditor.Model.Editor exposing (Editor)
import Session exposing (Session)


type alias Model =
    { session : Session
    , editor : FullEditor.Model
    }


type Msg
    = Msg
    | EditorMsg FullEditor.EditorMsg
    | GotSession Session


dummyView : { title : String, content : List (Html msg) }
dummyView =
    { title = "Dummy Home", content = [ text "Dummy Home" ] }


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Home"
    , content =
        [ h1 []
            [ text "Elm package for building rich text editors" ]
        , Html.map EditorMsg (FullEditor.view model.editor)
        ]
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, editor = FullEditor.init }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( e, _ ) =
                    FullEditor.update editorMsg model.editor
            in
            -- TODO: hook up commands from full editor
            ( { model | editor = e }, Cmd.none )

        _ ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
