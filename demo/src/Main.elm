module Main exposing (..)

import Array
import BasicEditorControls exposing (EditorMsg(..), InsertImageModal, InsertLinkModal)
import BasicSpecs exposing (simpleSpec)
import Browser
import Html exposing (Html, div)
import Html.Attributes
import Rte.Commands exposing (backspaceKey, enterKey, inputEvent, key, returnKey)
import Rte.Editor exposing (internalUpdate)
import Rte.List exposing (ListType)
import Rte.Model exposing (ChildNodes(..), Editor, EditorAttribute(..), EditorBlockNode, EditorInlineLeaf(..), InternalEditorMsg(..))
import Rte.Spec exposing (emptySpec)


headerElements =
    [ "h1", "h2", "h3", "h4", "h5", "h6" ]



---- MODEL ----


type alias Model =
    { editor : Editor EditorMsg
    , insertLinkModal : InsertLinkModal
    , insertImageModal : InsertImageModal
    }


initialEditorNode : EditorBlockNode
initialEditorNode =
    { parameters =
        { name = "crazy_block"
        , attributes = []
        , marks = []
        }
    , childNodes = InlineLeafArray (Array.fromList [ TextLeaf { text = "This is some sample text", marks = [] } ])
    }


commandBindings =
    Rte.Commands.defaultCommandBindings


initEditor : Editor EditorMsg
initEditor =
    { renderCount = 0
    , bufferedEditorState = Nothing
    , completeRerenderCount = 0
    , selectionCount = 0
    , isComposing = False
    , decoder = InternalMsg
    , commandMap = commandBindings
    , spec = simpleSpec
    , editorState =
        { root = initialEditorNode
        , selection = Nothing
        }
    }


initInsertLinkModal : InsertLinkModal
initInsertLinkModal =
    { visible = False, href = "", title = "", editorState = Nothing }


initInsertImageModal : InsertImageModal
initInsertImageModal =
    { visible = False, src = "", alt = "", editorState = Nothing }


init : ( Model, Cmd Msg )
init =
    ( { editor = initEditor
      , insertImageModal = initInsertImageModal
      , insertLinkModal = initInsertLinkModal
      }
    , Cmd.none
    )



---- UPDATE ----


type alias Msg =
    EditorMsg


handleShowInsertLinkModal : Model -> Model
handleShowInsertLinkModal model =
    let
        insertLinkModal =
            model.insertLinkModal
    in
    { model
        | insertLinkModal =
            { insertLinkModal
                | visible = True
                , editorState = Just model.editor.editorState
            }
    }


handleInsertLink : Model -> Model
handleInsertLink model =
    let
        insertLinkModal =
            model.insertLinkModal
    in
    { model
        | editor = model.editor
        , insertLinkModal =
            { insertLinkModal
                | visible = False
                , editorState = Nothing
                , href = ""
                , title = ""
            }
    }


handleInsertImage : Model -> Model
handleInsertImage model =
    let
        insertImageModal =
            model.insertImageModal

        newEditor =
            case insertImageModal.editorState of
                Nothing ->
                    model.editor

                Just editorState ->
                    model.editor
    in
    { model
        | editor = newEditor
        , insertImageModal =
            { insertImageModal
                | visible = False
                , editorState = Nothing
                , src = ""
                , alt = ""
            }
    }


handleToggleStyle : String -> Model -> Model
handleToggleStyle style model =
    model


handleInsertCode : Model -> Model
handleInsertCode model =
    model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InternalMsg internalEditorMsg ->
            ( { model | editor = internalUpdate internalEditorMsg model.editor }, Cmd.none )

        ToggleStyle style ->
            ( handleToggleStyle style model, Cmd.none )

        InsertCode ->
            ( handleInsertCode model, Cmd.none )

        ShowInsertLinkModal ->
            ( handleShowInsertLinkModal model, Cmd.none )

        InsertLink ->
            ( handleInsertLink model, Cmd.none )

        UpdateLinkHref href ->
            ( handleUpdateLinkHref href model, Cmd.none )

        UpdateLinkTitle title ->
            ( handleUpdateLinkTitle title model, Cmd.none )

        ShowInsertImageModal ->
            ( handleShowInsertImageModal model, Cmd.none )

        InsertImage ->
            ( handleInsertImage model, Cmd.none )

        UpdateImageSrc src ->
            ( handleUpdateImageSrc src model, Cmd.none )

        UpdateImageAlt alt ->
            ( handleUpdateImageAlt alt model, Cmd.none )

        WrapInBlockQuote ->
            ( handleWrapBlockNode model, Cmd.none )

        InsertHorizontalRule ->
            ( handleInsertHorizontalRule model, Cmd.none )

        LiftOutOfBlock ->
            ( handleLiftBlock model, Cmd.none )

        ToggleBlock block ->
            ( handleToggleBlock block model, Cmd.none )

        WrapInList listType ->
            ( handleWrapInList listType model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleLiftBlock : Model -> Model
handleLiftBlock model =
    model


handleWrapInList : ListType -> Model -> Model
handleWrapInList listType model =
    model


handleToggleBlock : String -> Model -> Model
handleToggleBlock block model =
    model


handleWrapBlockNode : Model -> Model
handleWrapBlockNode model =
    model


handleInsertHorizontalRule : Model -> Model
handleInsertHorizontalRule model =
    model


handleShowInsertImageModal : Model -> Model
handleShowInsertImageModal model =
    let
        insertImageModal =
            model.insertImageModal
    in
    { model
        | insertImageModal =
            { insertImageModal
                | visible = True
                , editorState = Just model.editor.editorState
            }
    }


handleUpdateImageSrc : String -> Model -> Model
handleUpdateImageSrc src model =
    let
        insertImageModal =
            model.insertImageModal
    in
    { model | insertImageModal = { insertImageModal | src = src } }


handleUpdateImageAlt : String -> Model -> Model
handleUpdateImageAlt alt model =
    let
        insertImageModal =
            model.insertImageModal
    in
    { model | insertImageModal = { insertImageModal | alt = alt } }


handleUpdateLinkTitle : String -> Model -> Model
handleUpdateLinkTitle title model =
    let
        insertLinkModal =
            model.insertLinkModal
    in
    { model | insertLinkModal = { insertLinkModal | title = title } }


handleUpdateLinkHref : String -> Model -> Model
handleUpdateLinkHref href model =
    let
        insertLinkModal =
            model.insertLinkModal
    in
    { model | insertLinkModal = { insertLinkModal | href = href } }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "editor-container" ]
        [ BasicEditorControls.editorControlPanel []
        , Rte.Editor.renderEditor model.editor
        , BasicEditorControls.renderInsertLinkModal model.insertLinkModal
        , BasicEditorControls.renderInsertImageModal model.insertImageModal
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
