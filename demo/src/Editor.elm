module Editor exposing (..)

import Array
import Controls exposing (EditorMsg(..), InsertImageModal, InsertLinkModal, Style(..))
import ExtraMarks exposing (strikethrough, underline)
import Html exposing (Html, div)
import Html.Attributes
import RichTextEditor.Annotation exposing (selectable)
import RichTextEditor.Commands as Commands
    exposing
        ( insertBlockNode
        , lift
        , liftEmpty
        , splitBlockHeaderToNewParagraph
        , toggleBlock
        , toggleMarkOnInlineNodes
        , wrap
        )
import RichTextEditor.Decorations exposing (selectableDecoration)
import RichTextEditor.Editor as Editor exposing (applyCommand, applyNamedCommandList)
import RichTextEditor.List exposing (ListType, defaultListDefinition)
import RichTextEditor.Model.Attribute exposing (Attribute(..))
import RichTextEditor.Model.Command as Command exposing (CommandMap, inputEvent, key, set, transformCommand)
import RichTextEditor.Model.Decorations exposing (Decorations, addElementDecoration, emptyDecorations)
import RichTextEditor.Model.Editor exposing (Editor, editor, state)
import RichTextEditor.Model.Element exposing (element)
import RichTextEditor.Model.Keys exposing (enter, return)
import RichTextEditor.Model.Mark as Mark exposing (ToggleAction(..), mark, markOrderFromSpec)
import RichTextEditor.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , blockNode
        , fromBlockArray
        , inlineChildren
        , inlineElement
        , marksFromInline
        , plainText
        )
import RichTextEditor.Model.Selection exposing (anchorNode, focusNode, normalize)
import RichTextEditor.Model.Spec exposing (Spec)
import RichTextEditor.Model.State as State exposing (State)
import RichTextEditor.Node exposing (Node(..), anyRange)
import RichTextEditor.Specs
    exposing
        ( blockquote
        , bold
        , code
        , codeBlock
        , doc
        , heading
        , horizontalRule
        , image
        , italic
        , link
        , paragraph
        )
import Set


type alias EditorMsg =
    Controls.EditorMsg



---- MODEL ----


type alias Model =
    { editor : Editor
    , styles : List Style
    , insertLinkModal : InsertLinkModal
    , insertImageModal : InsertImageModal
    }


docInitNode : Block
docInitNode =
    blockNode
        (element doc [] Set.empty)
        (fromBlockArray (Array.fromList [ initialEditorNode ]))


initialEditorNode : Block
initialEditorNode =
    blockNode
        (element paragraph [] Set.empty)
        (inlineChildren (Array.fromList [ plainText "This is some sample text" ]))


initialState : State
initialState =
    State.state docInitNode Nothing


listCommandBindings =
    RichTextEditor.List.commandBindings RichTextEditor.List.defaultListDefinition


commandBindings =
    Command.combine
        listCommandBindings
        (Commands.defaultCommandBindings
            |> set [ inputEvent "insertParagraph", key [ enter ], key [ return ] ]
                [ ( "liftEmpty", transformCommand <| liftEmpty )
                , ( "splitBlockHeaderToNewParagraph"
                  , transformCommand <|
                        splitBlockHeaderToNewParagraph
                            [ "heading" ]
                            (element paragraph [] Set.empty)
                  )
                ]
        )


decorations =
    addElementDecoration "image" (selectableDecoration InternalMsg) <|
        addElementDecoration "horizontal_rule" (selectableDecoration InternalMsg) <|
            emptyDecorations


initEditor : State -> Editor
initEditor iState =
    editor iState


initInsertLinkModal : InsertLinkModal
initInsertLinkModal =
    { visible = False, href = "", title = "", editorState = Nothing }


initInsertImageModal : InsertImageModal
initInsertImageModal =
    { visible = False, src = "", alt = "", editorState = Nothing }


init : State -> Model
init iState =
    { editor = initEditor iState
    , styles = [ Bold, Italic ]
    , insertImageModal = initInsertImageModal
    , insertLinkModal = initInsertLinkModal
    }



---- UPDATE ----


type alias Msg =
    EditorMsg


handleShowInsertLinkModal : Spec -> Model -> Model
handleShowInsertLinkModal spec model =
    let
        insertLinkModal =
            model.insertLinkModal

        editorState =
            state model.editor
    in
    case State.selection editorState of
        Nothing ->
            model

        Just selection ->
            let
                normalizedSelection =
                    normalize selection

                hasLink =
                    anyRange
                        (\n ->
                            case n of
                                Inline il ->
                                    List.any (\m -> Mark.name m == "link") (marksFromInline il)

                                _ ->
                                    False
                        )
                        (anchorNode normalizedSelection)
                        (focusNode normalizedSelection)
                        (State.root editorState)
            in
            if hasLink then
                let
                    markOrder =
                        markOrderFromSpec spec

                    linkMark =
                        mark link [ StringAttribute "src" "" ]

                    newEditor =
                        Result.withDefault model.editor <|
                            applyCommand
                                ( "removeLink"
                                , transformCommand <|
                                    Commands.toggleMarkOnInlineNodes markOrder linkMark Remove
                                )
                                spec
                                model.editor
                in
                { model | editor = newEditor }

            else
                { model
                    | insertLinkModal =
                        { insertLinkModal
                            | visible = True
                            , editorState = Just editorState
                        }
                }


handleInsertLink : Spec -> Model -> Model
handleInsertLink spec model =
    let
        insertLinkModal =
            model.insertLinkModal

        newEditor =
            case insertLinkModal.editorState of
                Nothing ->
                    model.editor

                Just _ ->
                    let
                        attributes =
                            [ StringAttribute "href" insertLinkModal.href
                            , StringAttribute "title" insertLinkModal.title
                            ]

                        markOrder =
                            markOrderFromSpec spec

                        linkMark =
                            mark link attributes
                    in
                    Result.withDefault model.editor <|
                        applyCommand
                            ( "insertLink"
                            , transformCommand <|
                                Commands.toggleMarkOnInlineNodes markOrder linkMark Add
                            )
                            spec
                            model.editor
    in
    { model
        | editor = newEditor
        , insertLinkModal =
            { insertLinkModal
                | visible = False
                , editorState = Nothing
                , href = ""
                , title = ""
            }
    }


handleInsertImage : Spec -> Model -> Model
handleInsertImage spec model =
    let
        insertImageModal =
            model.insertImageModal

        newEditor =
            case insertImageModal.editorState of
                Nothing ->
                    model.editor

                Just _ ->
                    let
                        params =
                            element image
                                [ StringAttribute "src" insertImageModal.src
                                , StringAttribute "alt" insertImageModal.alt
                                ]
                                (Set.singleton selectable)

                        img =
                            inlineElement params []
                    in
                    Result.withDefault model.editor <|
                        applyCommand
                            ( "insertImage"
                            , transformCommand <|
                                Commands.insertInlineElement img
                            )
                            spec
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


handleToggleStyle : Style -> Spec -> Model -> Model
handleToggleStyle style spec model =
    let
        markDef =
            case style of
                Bold ->
                    bold

                Italic ->
                    italic

                Code ->
                    code

                Strikethrough ->
                    strikethrough

                Underline ->
                    underline

        markOrder =
            markOrderFromSpec spec
    in
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "toggleStyle"
                    , transformCommand <|
                        toggleMarkOnInlineNodes markOrder (mark markDef []) Flip
                    )
                    spec
                    model.editor
                )
    }


update : CommandMap -> Spec -> Msg -> Model -> ( Model, Cmd Msg )
update commandMap spec msg model =
    case msg of
        InternalMsg internalEditorMsg ->
            ( { model | editor = Editor.update commandMap spec internalEditorMsg model.editor }, Cmd.none )

        ToggleStyle style ->
            ( handleToggleStyle style spec model, Cmd.none )

        ShowInsertLinkModal ->
            ( handleShowInsertLinkModal spec model, Cmd.none )

        InsertLink ->
            ( handleInsertLink spec model, Cmd.none )

        UpdateLinkHref href ->
            ( handleUpdateLinkHref href model, Cmd.none )

        UpdateLinkTitle title ->
            ( handleUpdateLinkTitle title model, Cmd.none )

        ShowInsertImageModal ->
            ( handleShowInsertImageModal model, Cmd.none )

        InsertImage ->
            ( handleInsertImage spec model, Cmd.none )

        UpdateImageSrc src ->
            ( handleUpdateImageSrc src model, Cmd.none )

        UpdateImageAlt alt ->
            ( handleUpdateImageAlt alt model, Cmd.none )

        WrapInBlockQuote ->
            ( handleWrapBlockNode spec model, Cmd.none )

        InsertHorizontalRule ->
            ( handleInsertHorizontalRule spec model, Cmd.none )

        LiftOutOfBlock ->
            ( handleLiftBlock spec model, Cmd.none )

        ToggleBlock block ->
            ( handleToggleBlock spec block model, Cmd.none )

        WrapInList listType ->
            ( handleWrapInList spec listType model, Cmd.none )

        Noop ->
            ( model, Cmd.none )

        CaptionedImage _ _ ->
            ( model, Cmd.none )


handleLiftBlock : Spec -> Model -> Model
handleLiftBlock spec model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyNamedCommandList
                    [ ( "liftList"
                      , transformCommand <| RichTextEditor.List.lift defaultListDefinition
                      )
                    , ( "lift"
                      , transformCommand <| lift
                      )
                    ]
                    spec
                    model.editor
                )
    }


handleWrapInList : Spec -> ListType -> Model -> Model
handleWrapInList spec listType model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand ( "wrapList", transformCommand <| RichTextEditor.List.wrap defaultListDefinition listType ) spec model.editor)
    }


handleToggleBlock : Spec -> String -> Model -> Model
handleToggleBlock spec block model =
    let
        onParams =
            if block == "Code block" then
                element
                    codeBlock
                    []
                    Set.empty

            else
                element
                    heading
                    [ IntegerAttribute
                        "level"
                        (Maybe.withDefault 1 <| String.toInt (String.right 1 block))
                    ]
                    Set.empty

        offParams =
            element paragraph [] Set.empty
    in
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "toggleBlock"
                    , transformCommand <| toggleBlock [ "heading", "code_block", "paragraph" ] onParams offParams
                    )
                    spec
                    model.editor
                )
    }


handleWrapBlockNode : Spec -> Model -> Model
handleWrapBlockNode spec model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "wrapBlockquote"
                    , transformCommand <|
                        wrap
                            (\n -> n)
                            (element blockquote [] Set.empty)
                    )
                    spec
                    model.editor
                )
    }


handleInsertHorizontalRule : Spec -> Model -> Model
handleInsertHorizontalRule spec model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "insertHR"
                    , transformCommand <|
                        insertBlockNode
                            (blockNode
                                (element
                                    horizontalRule
                                    []
                                    (Set.fromList [ selectable ])
                                )
                                Leaf
                            )
                    )
                    spec
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
                , editorState = Just (state model.editor)
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


view : Decorations Msg -> CommandMap -> Spec -> Model -> Html Msg
view decorations_ commandMap spec model =
    div [ Html.Attributes.class "editor-container" ]
        [ Controls.editorControlPanel model.styles model.editor
        , Editor.view InternalMsg decorations_ commandMap spec model.editor
        , Controls.renderInsertLinkModal model.insertLinkModal
        , Controls.renderInsertImageModal model.insertImageModal
        ]
