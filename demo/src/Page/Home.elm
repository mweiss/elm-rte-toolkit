module Page.Home exposing (..)

import Html exposing (Html, text)
import Session exposing (Session)


type alias Model =
    { session : Session }


type Msg
    = Msg
    | GotSession Session


dummyView : { title : String, content : Html msg }
dummyView =
    { title = "Dummy Home", content = text "Dummy Home" }


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home", content = text "Home" }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
