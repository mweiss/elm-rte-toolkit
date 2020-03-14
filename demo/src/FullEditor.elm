module FullEditor exposing (..)

import Array
import BasicEditorControls exposing (EditorMsg(..), InsertImageModal, InsertLinkModal, Style(..))
import Html exposing (Html, div)
import Html.Attributes
import RichTextEditor.Commands as Commands exposing (insertBlockNode, lift, liftEmpty, splitBlockHeaderToNewParagraph, toggleBlock, toggleMarkOnInlineNodes, wrap)
import RichTextEditor.Decorations exposing (addElementDecoration, selectableDecoration)
import RichTextEditor.Editor exposing (internalUpdate)
import RichTextEditor.Internal.Editor exposing (applyCommand, applyNamedCommandList)
import RichTextEditor.List exposing (ListType, defaultListDefinition)
import RichTextEditor.MarkdownSpec as MarkdownSpec exposing (blockquote, bold, code, codeBlock, doc, heading, horizontalRule, image, italic, link, paragraph)
import RichTextEditor.Model.Annotation exposing (selectableAnnotation)
import RichTextEditor.Model.Attribute exposing (Attribute(..))
import RichTextEditor.Model.Command as Command exposing (inputEvent, key, set, transformCommand)
import RichTextEditor.Model.Editor exposing (Editor, editor, emptyDecorations, spec, state, withCommandMap, withDecorations)
import RichTextEditor.Model.Keys exposing (enterKey, returnKey)
import RichTextEditor.Model.Mark as Mark exposing (ToggleAction(..), mark)
import RichTextEditor.Model.Node exposing (BlockNode, ChildNodes(..), InlineLeaf(..), Node(..), blockArray, blockNode, elementParameters, inlineLeaf, inlineLeafArray, inlineLeafParameters, marksFromInlineLeaf, textLeafWithText)
import RichTextEditor.Model.Selection exposing (anchorNode, focusNode, normalize)
import RichTextEditor.Model.State as State exposing (State)
import RichTextEditor.Node exposing (anyRange, foldlRange)
import RichTextEditor.Spec exposing (markOrderFromSpec)
import Set


type alias EditorMsg =
    BasicEditorControls.EditorMsg



---- MODEL ----


type alias Model =
    { editor : Editor EditorMsg
    , insertLinkModal : InsertLinkModal
    , insertImageModal : InsertImageModal
    }


inlineImageNode : InlineLeaf
inlineImageNode =
    InlineLeaf <|
        inlineLeafParameters
            (elementParameters image [ StringAttribute "src" "logo.svg" ] <|
                Set.fromList [ selectableAnnotation ]
            )
            []


paragraphWithImage =
    blockNode
        (elementParameters paragraph [] Set.empty)
        (inlineLeafArray
            (Array.fromList
                [ textLeafWithText ""
                , inlineImageNode
                , textLeafWithText ""
                ]
            )
        )


doubleInitNode : BlockNode
doubleInitNode =
    blockNode
        (elementParameters doc [] Set.empty)
        (blockArray (Array.fromList [ initialEditorNode, paragraphWithImage, initialEditorNode ]))


initialEditorNode : BlockNode
initialEditorNode =
    blockNode
        (elementParameters paragraph [] Set.empty)
        (inlineLeafArray (Array.fromList [ textLeafWithText "This is some sample text" ]))


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
                            (elementParameters paragraph [] Set.empty)
                  )
                ]
        )



-- TODO: fix this!! add real mark decorations


decorations =
    addElementDecoration "image" selectableDecoration <|
        addElementDecoration "horizontal_rule" selectableDecoration <|
            emptyDecorations


initialState : State
initialState =
    State.state doubleInitNode Nothing


initEditor : Editor EditorMsg
initEditor =
    editor MarkdownSpec.spec initialState InternalMsg
        |> withDecorations decorations
        |> withCommandMap commandBindings


initInsertLinkModal : InsertLinkModal
initInsertLinkModal =
    { visible = False, href = "", title = "", editorState = Nothing }


initInsertImageModal : InsertImageModal
initInsertImageModal =
    { visible = False, src = "", alt = "", editorState = Nothing }


init : Model
init =
    { editor = initEditor
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
                            elementParameters image
                                [ StringAttribute "src" insertImageModal.src
                                , StringAttribute "alt" insertImageModal.alt
                                ]
                                (Set.singleton selectableAnnotation)

                        img =
                            inlineLeaf params []
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
                elementParameters
                    codeBlock
                    []
                    Set.empty

            else
                elementParameters
                    heading
                    [ IntegerAttribute
                        "level"
                        (Maybe.withDefault 1 <| String.toInt (String.right 1 block))
                    ]
                    Set.empty

        offParams =
            elementParameters paragraph [] Set.empty
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
                            (elementParameters blockquote [] Set.empty)
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
                                (elementParameters
                                    horizontalRule
                                    []
                                    (Set.fromList [ selectableAnnotation ])
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
        [ BasicEditorControls.editorControlPanel model.editor
        , RichTextEditor.Editor.renderEditor model.editor
        , BasicEditorControls.renderInsertLinkModal model.insertLinkModal
        , BasicEditorControls.renderInsertImageModal model.insertImageModal
        ]
