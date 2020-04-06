module Page.Home exposing (..)

import Array
import Controls exposing (EditorMsg(..))
import Editor
import Html exposing (Html, a, h1, h2, li, p, text, ul)
import Html.Attributes exposing (class, href)
import RichText.Definitions as Specs exposing (code, doc, paragraph)
import RichText.Editor as RTE
import RichText.Model.Element exposing (element)
import RichText.Model.Mark exposing (mark)
import RichText.Model.Node
    exposing
        ( Block
        , Inline(..)
        , block
        , blockChildren
        , inlineChildren
        , plainText
        )
import RichText.Model.State as State exposing (State)
import RichText.Model.Text as Text
import Session exposing (Session)


type alias Model =
    { session : Session
    , editor : Editor.Model
    }


type Msg
    = EditorMsg Editor.EditorMsg
    | GotSession Session


config =
    RTE.config
        { decorations = Editor.decorations
        , commandMap = Editor.commandBindings Specs.markdown
        , spec = Specs.markdown
        , toMsg = InternalMsg
        }


notFoundView : { title : String, content : List (Html msg) }
notFoundView =
    { title = "Not found", content = [ p [ class "not-found" ] [ text "I'm sorry, I couldn't find the content you were looking for." ] ] }


features : List { title : String, text : String }
features =
    [ { title = "Cross-browser support"
      , text = "Instead of relying on inconsistent contenteditable APIs, the package depends on other web standards, like mutation observers, that are supported in all evergreen browsers on both desktop and mobile"
      }
    , { title = "Customizable specification"
      , text = "You can define a document with a custom structure, without having to code the rules from scratch."
      }
    , { title = "100% functional"
      , text = "All logic defining the editor is in Elm.  You don't need to write any js at all, and the only javascript you need to include are some pre-requisite webcomponents to bridge the gap between Elm and web APIs it doesn't natively support yet."
      }
    , { title = "Fits into the Elm Architecture"
      , text = "This package follows the guidelines of the Elm architecture and can fit seemlessly into your application."
      }
    ]


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Home"
    , content =
        [ h1 [ class "main-header" ]
            [ text "Elm package for building rich text editors" ]
        , Html.map EditorMsg (Editor.view config model.editor)
        , h2 [] [ text "Features" ]
        , ul [ class "grid-list" ] <|
            List.map
                (\v ->
                    li []
                        [ h2 [] [ text v.title ]
                        , p []
                            [ text <|
                                v.text
                            ]
                        ]
                )
                features
        , h2 [] [ text "About" ]
        , p []
            [ text "Rich Text Editor Toolkit is an "
            , a [ href "https://github.com/mweiss/elm-rte-toolkit/blob/master/LICENSE" ]
                [ text "open source" ]
            , text " project that you are free to use commercially. The "
            , a [ href "https://github.com/mweiss/elm-rte-toolkit" ] [ text "source code is hosted on GitHub." ]
            ]
        , p []
            [ text "Contributions in the form of bug reports, pull requests, or thoughtful discussions in the "
            , a [ href "https://github.com/mweiss/elm-rte-toolkit/issues" ] [ text "GitHub issue tracker" ]
            , text " are welcome. Please see the "
            , a [ href "https://github.com/mweiss/elm-rte-toolkit/blob/master/CODE_OF_CONDUCT.md" ] [ text "Code of Conduct" ]
            , text " for our pledge to contributors."
            ]
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
                    Editor.update config editorMsg model.editor
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
        (element doc [])
        (blockChildren (Array.fromList [ initialEditorNode ]))


initialEditorNode : Block
initialEditorNode =
    block
        (element paragraph [])
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
                        ++ "to detect changes and update itself.  The editor's model is defined "
                        ++ "and validated by a programmable specification that allows you to create a "
                        ++ "custom tailored editor that fits your needs."
                ]
            )
        )


initialState : State
initialState =
    State.state initNode Nothing
