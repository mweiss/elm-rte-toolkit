module Page.Basic exposing (..)

import BasicEditor
import Html exposing (Html, a, h1, p, text)
import Html.Attributes exposing (href, title)
import Links exposing (rteToolkit)
import Session exposing (Session)


type alias Model =
    { session : Session
    , editor : BasicEditor.Model
    }


type Msg
    = Msg
    | EditorMsg BasicEditor.EditorMsg
    | GotSession Session


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Basic"
    , content =
        [ h1 [] [ text "Basic example" ]
        , p []
            [ text """You can use this package to create all sorts of editors. Trying to write
                    one from scratch can be a little overwhelming though, so the package provides a
                    default spec and commands as a jumping off point for your own editors.
                    In this example, we use the default spec to create an editor which supports
                    things like headers, lists, as well as links and images."""
            ]
        , Html.map EditorMsg (BasicEditor.view model.editor)
        , p []
            [ text "You can see the code for this example in the "
            , a
                [ title "git repo"
                , href (rteToolkit ++ "/tree/master/demo/src/Page/Basic.elm")
                ]
                [ text "git repo." ]
            ]
        ]
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, editor = BasicEditor.init BasicEditor.initialState }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
