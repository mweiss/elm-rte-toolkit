module Editor exposing (..)

import Array
import Controls exposing (EditorMsg(..), InsertImageModal, InsertLinkModal, Style(..))
import ExtraMarks exposing (strikethrough, underline)
import Html exposing (Html, div)
import Html.Attributes
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
import RichTextEditor.Model.Annotations exposing (selectable)
import RichTextEditor.Model.Attribute exposing (Attribute(..))
import RichTextEditor.Model.Command as Command exposing (inputEvent, key, set, transformCommand)
import RichTextEditor.Model.Decorations exposing (Decorations, addElementDecoration, emptyDecorations)
import RichTextEditor.Model.Editor exposing (Editor, editor, spec, state, withCommandMap)
import RichTextEditor.Model.Element exposing (element)
import RichTextEditor.Model.Keys exposing (enterKey, returnKey)
import RichTextEditor.Model.Mark as Mark exposing (ToggleAction(..), mark)
import RichTextEditor.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , blockNode
        , fromBlockArray
        , inlineChildren
        , inlineElement
        , marksFromInlineLeaf
        , plainText
        )
import RichTextEditor.Model.Selection exposing (anchorNode, focusNode, normalize)
import RichTextEditor.Model.Spec exposing (Spec)
import RichTextEditor.Model.State as State exposing (State)
import RichTextEditor.Node exposing (Node(..), anyRange)
import RichTextEditor.Spec exposing (markOrderFromSpec)
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
    , decorations : Decorations EditorMsg
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
            |> set [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ]
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


initEditor : Spec -> State -> Editor
initEditor spec iState =
    editor spec iState
        |> withCommandMap commandBindings


initInsertLinkModal : InsertLinkModal
initInsertLinkModal =
    { visible = False, href = "", title = "", editorState = Nothing }


initInsertImageModal : InsertImageModal
initInsertImageModal =
    { visible = False, src = "", alt = "", editorState = Nothing }


init : State -> Spec -> Model
init iState spec =
    { editor = initEditor spec iState
    , decorations = decorations
    , styles = [ Bold, Italic ]
    , insertImageModal = initInsertImageModal
    , insertLinkModal = initInsertLinkModal
    }



---- UPDATE ----


type alias Msg =
    EditorMsg


handleShowInsertLinkModal : Model -> Model
handleShowInsertLinkModal model =
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
                                    List.any (\m -> Mark.name m == "link") (marksFromInlineLeaf il)

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
                        markOrderFromSpec (spec model.editor)

                    linkMark =
                        mark link [ StringAttribute "src" "" ]

                    newEditor =
                        Result.withDefault model.editor <|
                            applyCommand
                                ( "removeLink"
                                , transformCommand <|
                                    Commands.toggleMarkOnInlineNodes markOrder linkMark Remove
                                )
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


handleInsertLink : Model -> Model
handleInsertLink model =
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
                            markOrderFromSpec (spec model.editor)

                        linkMark =
                            mark link attributes
                    in
                    Result.withDefault model.editor <|
                        applyCommand
                            ( "insertLink"
                            , transformCommand <|
                                Commands.toggleMarkOnInlineNodes markOrder linkMark Add
                            )
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


handleInsertImage : Model -> Model
handleInsertImage model =
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


handleToggleStyle : Style -> Model -> Model
handleToggleStyle style model =
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
            markOrderFromSpec (spec model.editor)
    in
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "toggleStyle"
                    , transformCommand <|
                        toggleMarkOnInlineNodes markOrder (mark markDef []) Flip
                    )
                    model.editor
                )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InternalMsg internalEditorMsg ->
            ( { model | editor = Editor.update internalEditorMsg model.editor }, Cmd.none )

        ToggleStyle style ->
            ( handleToggleStyle style model, Cmd.none )

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

        Noop ->
            ( model, Cmd.none )

        CaptionedImage _ _ ->
            ( model, Cmd.none )


handleLiftBlock : Model -> Model
handleLiftBlock model =
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
                    model.editor
                )
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
                            (element blockquote [] Set.empty)
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
                            (blockNode
                                (element
                                    horizontalRule
                                    []
                                    (Set.fromList [ selectable ])
                                )
                                Leaf
                            )
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


view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "editor-container" ]
        [ Controls.editorControlPanel model.styles model.editor
        , Editor.view InternalMsg model.decorations model.editor
        , Controls.renderInsertLinkModal model.insertLinkModal
        , Controls.renderInsertImageModal model.insertImageModal
        ]
