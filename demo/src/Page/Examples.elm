module Page.Examples exposing (..)

import Html exposing (Html, a, h1, h2, li, p, text, ul)
import Html.Attributes exposing (class)
import Route exposing (Route)
import Session exposing (Session)


type alias Model =
    { session : Session }


type Msg
    = GotSession Session


values : List { title : String, route : Route, text : String }
values =
    [ { title = "Basics"
      , text =
            "This example shows how to set up a minimal "
                ++ "rich text editor with the default configuration."
      , route = Route.Basic
      }
    , { title = "Markdown"
      , text =
            "This example shows how you can switch between a "
                ++ "plain markdown editor and a fancier rich text editor."
      , route = Route.Markdown
      }
    , { title = "Extend a specification"
      , text =
            "This example shows how you can extend the default specification "
                ++ "with your own mark and element definitions."
      , route = Route.SpecExtension
      }
    , { title = "New specification"
      , text = "This example shows how you can create a new document specification from scratch."
      , route = Route.SpecFromScratch
      }
    ]


view : Model -> { title : String, content : List (Html Msg) }
view _ =
    { title = "Examples"
    , content =
        [ h1 [] [ text "Examples of some of the things this toolkit can do" ]
        , ul [ class "grid-list" ] <|
            List.map
                (\v ->
                    li []
                        [ a [ class "blocklink", Route.href v.route ]
                            [ h2 [] [ text v.title ]
                            , p []
                                [ text <|
                                    v.text
                                ]
                            ]
                        ]
                )
                values
        ]
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
