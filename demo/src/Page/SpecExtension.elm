module Page.SpecExtension exposing (..)

import Array
import Controls exposing (EditorMsg(..), Style(..))
import Editor
import ExtraMarks exposing (strikethrough, underline)
import Html exposing (Html, a, button, div, h1, p, text)
import Html.Attributes exposing (href, title)
import Html.Events exposing (onClick)
import Json.Decode as D
import Links exposing (rteToolkit)
import RichText.Commands as Commands exposing (toggleMark)
import RichText.Config.Command exposing (CommandMap, Transform, inputEvent, key, set, transform)
import RichText.Config.Decorations exposing (addElementDecoration, selectableDecoration)
import RichText.Config.ElementDefinition
    exposing
        ( ElementDefinition
        , ElementToHtml
        , HtmlToElement
        , blockLeaf
        , elementDefinition
        )
import RichText.Config.Keys exposing (short)
import RichText.Config.Spec
    exposing
        ( Spec
        , elementDefinitions
        , markDefinitions
        , withElementDefinitions
        , withMarkDefinitions
        )
import RichText.Definitions as MarkdownSpec exposing (doc, paragraph)
import RichText.Editor as RTE exposing (apply, applyNoForceSelection)
import RichText.Model.Attribute
    exposing
        ( Attribute(..)
        , findStringAttribute
        , replaceOrAddStringAttribute
        )
import RichText.Model.Element as Element exposing (Element, element)
import RichText.Model.HtmlNode exposing (HtmlNode(..))
import RichText.Model.Mark exposing (ToggleAction(..), mark, markOrderFromSpec)
import RichText.Model.Node as Node
    exposing
        ( Block
        , Children(..)
        , Path
        , block
        , blockChildren
        , inlineChildren
        , plainText
        , withElement
        )
import RichText.Model.State as State exposing (State, withRoot)
import RichText.Node as Node exposing (Node(..), nodeAt)
import Session exposing (Session)


type alias Model =
    { session : Session
    , editor : Editor.Model
    , insertCaptionedImageModal : InsertCaptionedImageModal
    }


type alias InsertCaptionedImageModal =
    { visible : Bool
    , editorState : Maybe State
    , src : String
    , alt : String
    , caption : String
    }


type Msg
    = ShowUpdateCaptionedImageModel
    | UpdateCaptionedImageSrc String
    | UpdateCaptionedImageAlt String
    | UpdateCaption String
    | InsertCaptionedImage
    | EditorMsg Editor.EditorMsg
    | GotSession Session


config : RTE.Config EditorMsg
config =
    RTE.config
        { decorations = newDecorations
        , commandMap = commandBindings customSpec
        , spec = customSpec
        , toMsg = InternalMsg
        }


commandBindings : Spec -> CommandMap
commandBindings spec =
    let
        markOrder =
            markOrderFromSpec spec
    in
    Editor.commandBindings customSpec
        |> set [ inputEvent "formatUnderline", key [ short, "u" ] ]
            [ ( "toggleStyle", transform <| toggleMark markOrder (mark underline []) Flip )
            ]
        |> set [ inputEvent "formatStrikeThrough", key [ short, "d" ] ]
            [ ( "toggleStyle", transform <| toggleMark markOrder (mark strikethrough []) Flip )
            ]


handleShowInsertCaptionedImageModal : Model -> Model
handleShowInsertCaptionedImageModal model =
    let
        insertImageModal =
            model.insertCaptionedImageModal
    in
    { model
        | insertCaptionedImageModal =
            { insertImageModal
                | visible = True
                , editorState = Just (RTE.state model.editor.editor)
            }
    }


handleUpdateCaptionedImageSrc : String -> Model -> Model
handleUpdateCaptionedImageSrc src model =
    let
        insertImageModal =
            model.insertCaptionedImageModal
    in
    { model | insertCaptionedImageModal = { insertImageModal | src = src } }


handleUpdateCaptionedImageAlt : String -> Model -> Model
handleUpdateCaptionedImageAlt alt model =
    let
        insertImageModal =
            model.insertCaptionedImageModal
    in
    { model | insertCaptionedImageModal = { insertImageModal | alt = alt } }


handleUpdateCaption : String -> Model -> Model
handleUpdateCaption caption model =
    let
        insertImageModal =
            model.insertCaptionedImageModal
    in
    { model | insertCaptionedImageModal = { insertImageModal | caption = caption } }


handleInsertCaptionedImage : Spec -> Model -> Model
handleInsertCaptionedImage spec model =
    let
        insertImageModal =
            model.insertCaptionedImageModal

        editor =
            model.editor

        newEditor =
            case insertImageModal.editorState of
                Nothing ->
                    model.editor.editor

                Just state_ ->
                    let
                        params =
                            element captionedImage
                                [ StringAttribute "src" insertImageModal.src
                                , StringAttribute "alt" insertImageModal.alt
                                , StringAttribute "caption" insertImageModal.caption
                                ]

                        img =
                            block params Leaf
                    in
                    Result.withDefault model.editor.editor <|
                        apply
                            ( "insertImage"
                            , transform <|
                                Editor.setResult (Commands.insertBlock img state_)
                            )
                            spec
                            model.editor.editor
    in
    { model
        | editor = { editor | editor = newEditor }
        , insertCaptionedImageModal =
            { insertImageModal
                | visible = False
                , editorState = Nothing
                , src = ""
                , alt = ""
                , caption = ""
            }
    }


renderInsertCaptionedImageModal : InsertCaptionedImageModal -> Html Msg
renderInsertCaptionedImageModal insertImageModal =
    Controls.modal insertImageModal.visible
        [ Html.h3 []
            [ Html.text "Insert captioned image" ]
        , Html.div
            []
            [ Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.name "src"
                , Html.Attributes.value insertImageModal.src
                , Html.Attributes.placeholder "Image URL (ex: https://via.placeholder.com/150.png)"
                , Html.Events.onInput UpdateCaptionedImageSrc
                ]
                []
            ]
        , Html.div
            []
            [ Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.name "alt"
                , Html.Attributes.value insertImageModal.alt
                , Html.Attributes.placeholder "Alt text"
                , Html.Events.onInput UpdateCaptionedImageAlt
                ]
                []
            ]
        , Html.div
            []
            [ Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.name "caption"
                , Html.Attributes.value insertImageModal.alt
                , Html.Attributes.placeholder "Caption"
                , Html.Events.onInput UpdateCaption
                ]
                []
            ]
        , Html.div
            []
            [ Html.button
                [ Html.Events.onClick InsertCaptionedImage ]
                [ Html.text "Insert" ]
            ]
        ]


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Extending a specification"
    , content =
        [ h1 [] [ text "Extending a specification" ]
        , p []
            [ text """This example shows how you can extend a specification.  Namely, we add two
            extra marks for strikethrough and underline, and we add a new block leaf element to
            display a captioned image."""
            ]
        , p []
            [ text "You can see the code for this example in the "
            , a
                [ title "git repo"
                , href (rteToolkit ++ "/tree/master/demo/src/Page/SpecExtension.elm")
                ]
                [ text "git repo." ]
            ]
        , captionedImageView model
        , Html.map EditorMsg (Editor.view config model.editor)
        ]
    }


captionedImageView : Model -> Html Msg
captionedImageView model =
    div []
        [ button [ onClick ShowUpdateCaptionedImageModel ] [ text "Insert captioned image" ]
        , renderInsertCaptionedImageModal model.insertCaptionedImageModal
        ]


customSpec : Spec
customSpec =
    MarkdownSpec.markdown
        |> withElementDefinitions (elementDefinitions MarkdownSpec.markdown ++ [ captionedImage ])
        |> withMarkDefinitions
            (markDefinitions MarkdownSpec.markdown
                ++ [ strikethrough, underline ]
            )


docInitNode : Block
docInitNode =
    block
        (element doc [])
        (blockChildren (Array.fromList [ loremParagraph, initialCaptionedImage, loremParagraph ]))


loremParagraph : Block
loremParagraph =
    block
        (element paragraph [])
        (inlineChildren (Array.fromList [ plainText "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." ]))


initialCaptionedImage : Block
initialCaptionedImage =
    block
        (element captionedImage
            [ StringAttribute "caption" "The elm logo!", StringAttribute "src" "logo.svg" ]
        )
        Leaf


initialState : State
initialState =
    State.state docInitNode Nothing


newDecorations =
    Editor.decorations |> addElementDecoration captionedImage preventKeyDownPropagationDecoration


init : Session -> ( Model, Cmd Msg )
init session =
    let
        editor =
            Editor.init initialState

        newEditor =
            { editor
                | styles = [ Bold, Italic, Strikethrough, Underline ]
            }
    in
    ( { session = session
      , editor = newEditor
      , insertCaptionedImageModal =
            { visible = False
            , editorState = Nothing
            , src = ""
            , alt = ""
            , caption = ""
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            case editorMsg of
                CaptionedImage path s ->
                    ( { model | editor = handleCaptionedImageText path s model.editor }, Cmd.none )

                _ ->
                    let
                        ( e, _ ) =
                            Editor.update config editorMsg model.editor
                    in
                    ( { model | editor = e }, Cmd.none )

        ShowUpdateCaptionedImageModel ->
            ( handleShowInsertCaptionedImageModal model, Cmd.none )

        UpdateCaptionedImageSrc s ->
            ( handleUpdateCaptionedImageSrc s model, Cmd.none )

        UpdateCaptionedImageAlt s ->
            ( handleUpdateCaptionedImageAlt s model, Cmd.none )

        UpdateCaption s ->
            ( handleUpdateCaption s model, Cmd.none )

        InsertCaptionedImage ->
            ( handleInsertCaptionedImage customSpec model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateCaptionedImageText : Path -> String -> Transform
updateCaptionedImageText path value state =
    let
        r =
            State.root state
    in
    case nodeAt path (State.root state) of
        Nothing ->
            Err "There is no node at the given path"

        Just node ->
            case node of
                Inline _ ->
                    Err "I can only update captioned images, but I received an inline node"

                Block bn ->
                    let
                        ep =
                            Node.element bn

                        attributes =
                            Element.attributes ep

                        newAttributes =
                            replaceOrAddStringAttribute "caption" value attributes

                        newElementParameters =
                            ep |> Element.withAttributes newAttributes

                        newBlockNode =
                            bn |> withElement newElementParameters
                    in
                    if Element.name ep /= "captioned_image" then
                        Err "I received a node that was not a captioned image"

                    else
                        case Node.replace path (Block newBlockNode) r of
                            Err s ->
                                Err s

                            Ok newRoot ->
                                Ok (state |> withRoot newRoot)


handleCaptionedImageText : Path -> String -> Editor.Model -> Editor.Model
handleCaptionedImageText path value model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyNoForceSelection
                    ( "updateCaptionedImageText"
                    , transform <|
                        updateCaptionedImageText
                            path
                            value
                    )
                    customSpec
                    model.editor
                )
    }


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


captionedImage : ElementDefinition
captionedImage =
    elementDefinition
        { name = "captioned_image"
        , group = "block"
        , contentType = blockLeaf
        , toHtmlNode = imageToHtmlNode
        , fromHtmlNode = htmlNodeToImage
        , selectable = True
        }


imageToHtmlNode : ElementToHtml
imageToHtmlNode parameters _ =
    let
        caption =
            Maybe.withDefault
                ""
                (findStringAttribute "caption" (Element.attributes parameters))

        attributes =
            List.filterMap identity
                [ Just <| ( "src", Maybe.withDefault "" (findStringAttribute "src" (Element.attributes parameters)) )
                , Maybe.map (\x -> ( "alt", x )) (findStringAttribute "alt" (Element.attributes parameters))
                , Maybe.map (\x -> ( "title", x )) (findStringAttribute "title" (Element.attributes parameters))
                , Just ( "data-caption", caption )
                ]
    in
    ElementNode "figure"
        [ ( "contenteditable", "false" ) ]
        (Array.fromList
            [ ElementNode "img"
                attributes
                Array.empty
            , ElementNode "figcaption"
                []
                (Array.fromList
                    [ ElementNode "input"
                        [ ( "value", caption )
                        , ( "type", "text" )
                        , ( "class", "caption" )
                        , ( "placeholder", "Add a caption..." )
                        ]
                        Array.empty
                    ]
                )
            ]
        )


parseImageAttributes : HtmlNode -> Maybe (List Attribute)
parseImageAttributes node =
    case node of
        ElementNode name attributes _ ->
            if name == "img" then
                Just <|
                    List.filterMap
                        (\( k, v ) ->
                            case k of
                                "src" ->
                                    Just <| StringAttribute "src" v

                                "alt" ->
                                    Just <| StringAttribute "alt" v

                                "title" ->
                                    Just <| StringAttribute "title" v

                                "data-caption" ->
                                    Just <| StringAttribute "caption" v

                                _ ->
                                    Nothing
                        )
                        attributes

            else
                Nothing

        _ ->
            Nothing


htmlNodeToImage : HtmlToElement
htmlNodeToImage def node =
    case node of
        ElementNode name _ children ->
            if name == "figure" then
                case Array.get 0 children of
                    Nothing ->
                        Nothing

                    Just img ->
                        case parseImageAttributes img of
                            Nothing ->
                                Nothing

                            Just attr ->
                                Just
                                    ( element def attr
                                    , Array.empty
                                    )

            else
                Nothing

        _ ->
            Nothing


preventKeyDownPropagationDecoration : Path -> Element -> Path -> List (Html.Attribute EditorMsg)
preventKeyDownPropagationDecoration editorNodePath elementParameters elementPath =
    if elementPath == [] then
        selectableDecoration InternalMsg editorNodePath elementParameters elementPath

    else if elementPath == [ 1, 0 ] then
        [ Html.Events.stopPropagationOn "keydown" (D.succeed ( Noop, True ))
        , Html.Events.stopPropagationOn "beforeinput" (D.succeed ( Noop, True ))
        , Html.Events.onInput (CaptionedImage editorNodePath)
        ]

    else
        []
