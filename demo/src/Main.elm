module Main exposing (..)

import Array
import BasicEditorControls exposing (EditorMsg(..), InsertImageModal, InsertLinkModal)
import BasicSpecs exposing (simpleSpec)
import BoundedDeque
import Browser
import Html exposing (Html, div)
import Html.Attributes
import RichTextEditor.Commands exposing (enterKey, inputEvent, insertBlockNode, key, lift, liftEmpty, returnKey, set, splitBlockHeaderToNewParagraph, toggleBlock, toggleMarkOnInlineNodes, wrap)
import RichTextEditor.Decorations exposing (addElementDecoration, emptyDecorations, selectableDecoration)
import RichTextEditor.Editor exposing (applyCommand, internalUpdate)
import RichTextEditor.Internal.Model exposing (ChildNodes(..), Editor, EditorAttribute(..), EditorBlockNode, EditorInlineLeaf(..), InternalEditorMsg(..), Mark, elementParameters, inlineLeafArray, selectableAnnotation, transformCommand)
import RichTextEditor.List exposing (ListType, defaultListDefinition)
import RichTextEditor.Spec exposing (markOrderFromSpec)
import Set



---- MODEL ----


type alias Model =
    { editor : Editor EditorMsg
    , insertLinkModal : InsertLinkModal
    , insertImageModal : InsertImageModal
    }


inlineImageNode : EditorInlineLeaf
inlineImageNode =
    InlineLeaf
        { marks = []
        , parameters =
            elementParameters "image" [ StringAttribute "src" "logo.svg" ] <|
                Set.fromList [ selectableAnnotation ]
        }


paragraphWithImage =
    { parameters = elementParameters "paragraph" [] Set.empty
    , childNodes =
        inlineLeafArray
            (Array.fromList
                [ TextLeaf { text = "", marks = [], annotations = Set.empty }
                , inlineImageNode
                , TextLeaf { text = "", marks = [], annotations = Set.empty }
                ]
            )
    }


doubleInitNode : EditorBlockNode
doubleInitNode =
    { parameters = elementParameters "doc" [] Set.empty
    , childNodes = BlockArray (Array.fromList [ initialEditorNode, paragraphWithImage, initialEditorNode ])
    }


initialEditorNode : EditorBlockNode
initialEditorNode =
    { parameters = elementParameters "paragraph" [] Set.empty
    , childNodes = inlineLeafArray (Array.fromList [ TextLeaf { text = "This is some sample text", marks = [], annotations = Set.empty } ])
    }


listCommandBindings =
    RichTextEditor.List.commandBindings RichTextEditor.List.defaultListDefinition


commandBindings =
    RichTextEditor.Commands.combine
        listCommandBindings
        (RichTextEditor.Commands.defaultCommandBindings
            |> set [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ]
                [ ( "liftEmpty", transformCommand <| liftEmpty )
                , ( "splitBlockHeaderToNewParagraph"
                  , transformCommand <|
                        splitBlockHeaderToNewParagraph [ "header" ] "p"
                  )
                ]
        )



-- TODO: fix this!! add real mark decorations


decorations =
    addElementDecoration "image" selectableDecoration <|
        addElementDecoration "horizontal_rule" selectableDecoration <|
            emptyDecorations


dequeSize : Int
dequeSize =
    1024


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
    , history = { undoDeque = BoundedDeque.empty dequeSize, redoStack = [] }
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

        markOrder =
            markOrderFromSpec model.editor.spec
    in
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "toggleStyle"
                    , transformCommand <|
                        toggleMarkOnInlineNodes markOrder (Mark markName [])
                    )
                    model.editor
                )
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
                (applyCommand ( "lift", transformCommand <| lift ) model.editor)
    }


handleWrapInList : ListType -> Model -> Model
handleWrapInList listType model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand ( "wrapList", transformCommand <| RichTextEditor.List.wrap defaultListDefinition listType ) model.editor)
    }


handleToggleBlock : String -> Model -> Model
handleToggleBlock block model =
    let
        onParams =
            if block == "Code block" then
                elementParameters
                    "code_block"
                    []
                    Set.empty

            else
                elementParameters
                    "heading"
                    [ IntegerAttribute
                        "level"
                        (Maybe.withDefault 1 <| String.toInt (String.right 1 block))
                    ]
                    Set.empty

        offParams =
            elementParameters "paragraph" [] Set.empty
    in
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "toggleBlock"
                    , transformCommand <| toggleBlock [ "heading", "code_block", "paragraph" ] onParams offParams
                    )
                    model.editor
                )
    }


handleWrapBlockNode : Model -> Model
handleWrapBlockNode model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "wrapBlockquote"
                    , transformCommand <|
                        wrap
                            (\n -> n)
                            (elementParameters "blockquote" [] Set.empty)
                    )
                    model.editor
                )
    }


handleInsertHorizontalRule : Model -> Model
handleInsertHorizontalRule model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "insertHR"
                    , transformCommand <|
                        insertBlockNode
                            { parameters =
                                elementParameters
                                    "horizontal_rule"
                                    []
                                    (Set.fromList [ selectableAnnotation ])
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
        , RichTextEditor.Editor.renderEditor model.editor
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
