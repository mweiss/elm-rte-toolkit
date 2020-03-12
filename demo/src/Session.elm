port module Session exposing (..)

import Browser.Navigation as Nav
import Json.Encode exposing (Value)


type Session
    = Session Nav.Key


navKey : Session -> Nav.Key
navKey session =
    case session of
        Session key ->
            key


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    onStoreChange (\_ -> toMsg (Session key))


port onStoreChange : (Value -> msg) -> Sub msg
