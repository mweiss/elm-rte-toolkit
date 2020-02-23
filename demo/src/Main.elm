module Main exposing (..)

import Array
import BasicEditorControls exposing (EditorMsg(..), InsertImageModal, InsertLinkModal)
import BasicSpecs exposing (simpleSpec)
import Browser
import Html exposing (Html, div)
import Html.Attributes
import Rte.Commands exposing (enterKey, inputEvent, insertBlockNode, key, lift, liftEmpty, otherwiseDo, returnKey, set, splitBlockHeaderToNewParagraph, toggleBlock, toggleMarkOnInlineNodes, wrapIn)
import Rte.Decorations exposing (addElementDecoration, emptyDecorations, selectableDecoration)
import Rte.Editor exposing (internalUpdate)
import Rte.EditorUtils exposing (applyCommand)
import Rte.List exposing (ListType)
import Rte.Model exposing (ChildNodes(..), Editor, EditorAttribute(..), EditorBlockNode, EditorInlineLeaf(..), InternalEditorMsg(..), Mark, selectableMark)


headerElements =
    [ "h1", "h2", "h3", "h4", "h5", "h6" ]



---- MODEL ----


type alias Model =
    { editor : Editor EditorMsg
    , insertLinkModal : InsertLinkModal
    , insertImageModal : InsertImageModal
    }


inlineImageNode : EditorInlineLeaf
inlineImageNode =
    InlineLeaf
        { name = "img"
        , attributes = [ StringAttribute "src" "logo.svg" ]
        , marks = [ selectableMark ]
        }


paragraphWithImage =
    { parameters =
        { name = "p"
        , attributes = []
        , marks = []
        }
    , childNodes = InlineLeafArray (Array.fromList [ TextLeaf { text = "", marks = [] }, inlineImageNode, TextLeaf { text = "", marks = [] } ])
    }


doubleInitNode : EditorBlockNode
doubleInitNode =
    { parameters =
        { name = "div"
        , attributes = []
        , marks = []
        }
    , childNodes = BlockArray (Array.fromList [ initialEditorNode, paragraphWithImage, initialEditorNode ])
    }


initialEditorNode : EditorBlockNode
initialEditorNode =
    { parameters =
        { name = "p"
        , attributes = []
        , marks = []
        }
    , childNodes = InlineLeafArray (Array.fromList [ TextLeaf { text = "This is some sample text", marks = [] } ])
    }


commandBindings =
    Rte.Commands.defaultCommandBindings
        |> set [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ] (liftEmpty |> otherwiseDo (splitBlockHeaderToNewParagraph headerElements "p"))



-- TODO: fix this!! add real mark decorations


decorations =
    addElementDecoration "img" selectableDecoration <|
        addElementDecoration "hr" selectableDecoration <|
            emptyDecorations


initEditor : Editor EditorMsg
initEditor =
    { renderCount = 0
    , bufferedEditorState = Nothing
    , completeRerenderCount = 0
    , selectionCount = 0
    , isComposing = False
    , decoder = InternalMsg
    , decorations = decorations
    , commandMap = commandBindings
    , spec = simpleSpec
    , editorState =
        { root = doubleInitNode
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
    let
        markName =
            case style of
                "Bold" ->
                    "bold"

                "Italic" ->
                    "italic"

                _ ->
                    "bold"
    in
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand (toggleMarkOnInlineNodes (Mark markName [])) model.editor)
    }


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
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand lift model.editor)
    }


handleWrapInList : ListType -> Model -> Model
handleWrapInList listType model =
    model


handleToggleBlock : String -> Model -> Model
handleToggleBlock block model =
    let
        blockName =
            String.toLower block

        tagName =
            if blockName == "code block" then
                "code_block"

            else
                blockName
    in
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand (toggleBlock (headerElements ++ [ "code_block", "p" ]) tagName "p") model.editor)
    }


handleWrapBlockNode : Model -> Model
handleWrapBlockNode model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand (wrapIn { name = "blockquote", marks = [], attributes = [] }) model.editor)
    }


handleInsertHorizontalRule : Model -> Model
handleInsertHorizontalRule model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    (insertBlockNode
                        { parameters = { name = "hr", marks = [ selectableMark ], attributes = [] }
                        , childNodes = Leaf
                        }
                    )
                    model.editor
                )
    }


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
