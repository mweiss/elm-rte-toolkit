module Editor exposing (..)

import Array
import Controls exposing (EditorMsg(..), InsertImageModal, InsertLinkModal, Style(..))
import ExtraMarks exposing (strikethrough, underline)
import Html exposing (Html, div)
import Html.Attributes
import RichText.Commands as Commands exposing (insertAfterBlockLeaf, insertBlock, insertNewline, lift, liftEmpty, splitBlockHeaderToNewParagraph, toggleMark, toggleTextBlock, wrap)
import RichText.Config.Command as Command exposing (CommandMap, inputEvent, internal, key, set, transform)
import RichText.Config.Decorations
    exposing
        ( Decorations
        , addElementDecoration
        , emptyDecorations
        , selectableDecoration
        )
import RichText.Config.Keys exposing (enter, return, short)
import RichText.Config.Spec exposing (Spec)
import RichText.Definitions
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
import RichText.Editor as Editor exposing (Config, Editor, apply, applyList, state)
import RichText.List exposing (ListType, defaultListDefinition)
import RichText.Model.Attribute exposing (Attribute(..))
import RichText.Model.Element as Element exposing (element)
import RichText.Model.Mark as Mark exposing (ToggleAction(..), mark, markOrderFromSpec)
import RichText.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , block
        , blockChildren
        , inlineChildren
        , inlineElement
        , marks
        , plainText
        )
import RichText.Model.Selection exposing (anchorNode, focusNode, normalize)
import RichText.Model.State as State exposing (State)
import RichText.Node exposing (Node(..), anyRange)


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
    block
        (element doc [])
        (blockChildren (Array.fromList [ initialEditorNode ]))


initialEditorNode : Block
initialEditorNode =
    block
        (element paragraph [])
        (inlineChildren (Array.fromList [ plainText "This is some sample text" ]))


initialState : State
initialState =
    State.state docInitNode Nothing


listCommandBindings =
    RichText.List.defaultCommandMap RichText.List.defaultListDefinition


emptyParagraph : Block
emptyParagraph =
    block
        (Element.element paragraph [])
        (inlineChildren <| Array.fromList [ plainText "" ])


commandBindings : Spec -> CommandMap
commandBindings spec =
    let
        markOrder =
            markOrderFromSpec spec
    in
    Command.combine
        listCommandBindings
        (Commands.defaultCommandMap
            |> set [ inputEvent "insertParagraph", key [ enter ], key [ return ] ]
                [ ( "insertNewline"
                  , transform <| insertNewline [ "code_block" ]
                  )
                , ( "liftEmpty", transform <| liftEmpty )
                , ( "splitBlockHeaderToNewParagraph"
                  , transform <|
                        splitBlockHeaderToNewParagraph
                            [ "heading" ]
                            (element paragraph [])
                  )
                , ( "insertEmptyParagraph"
                  , transform <|
                        insertAfterBlockLeaf emptyParagraph
                  )
                ]
            |> set [ inputEvent "formatBold", key [ short, "b" ] ]
                [ ( "toggleStyle", transform <| toggleMark markOrder (mark bold []) Flip )
                ]
            |> set [ inputEvent "formatItalic", key [ short, "i" ] ]
                [ ( "toggleStyle", transform <| toggleMark markOrder (mark italic []) Flip )
                ]
        )


decorations =
    emptyDecorations
        |> addElementDecoration image (selectableDecoration InternalMsg)
        |> addElementDecoration horizontalRule (selectableDecoration InternalMsg)


initEditor : State -> Editor
initEditor iState =
    Editor.init iState


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
                                    List.any (\m -> Mark.name m == "link") (marks il)

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
                            apply
                                ( "removeLink"
                                , transform <|
                                    Commands.toggleMark markOrder linkMark Remove
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


{-| Dummy transform that always returns the result it receives.
-}
setResult : Result String State -> State -> Result String State
setResult result _ =
    result


handleInsertLink : Spec -> Model -> Model
handleInsertLink spec model =
    let
        insertLinkModal =
            model.insertLinkModal

        newEditor =
            case insertLinkModal.editorState of
                Nothing ->
                    model.editor

                Just state_ ->
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
                        apply
                            ( "insertLink"
                            , transform <|
                                setResult (Commands.toggleMark markOrder linkMark Add state_)
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

                Just state_ ->
                    let
                        params =
                            element image
                                [ StringAttribute "src" insertImageModal.src
                                , StringAttribute "alt" insertImageModal.alt
                                ]

                        img =
                            inlineElement params []
                    in
                    Result.withDefault model.editor <|
                        apply
                            ( "insertImage"
                            , transform <|
                                setResult (Commands.insertInline img state_)
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
                (apply
                    ( "toggleStyle"
                    , transform <|
                        toggleMark markOrder (mark markDef []) Flip
                    )
                    spec
                    model.editor
                )
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd Msg )
update cfg msg model =
    let
        spec =
            Editor.spec cfg
    in
    case msg of
        InternalMsg internalEditorMsg ->
            ( { model | editor = Editor.update cfg internalEditorMsg model.editor }, Cmd.none )

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

        Undo ->
            ( handleUndo spec model, Cmd.none )

        Redo ->
            ( handleRedo spec model, Cmd.none )

        Noop ->
            ( model, Cmd.none )

        CaptionedImage _ _ ->
            ( model, Cmd.none )


handleLiftBlock : Spec -> Model -> Model
handleLiftBlock spec model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyList
                    [ ( "liftList"
                      , transform <| RichText.List.lift defaultListDefinition
                      )
                    , ( "lift"
                      , transform <| lift
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
                (apply ( "wrapList", transform <| RichText.List.wrap defaultListDefinition listType ) spec model.editor)
    }


handleRedo : Spec -> Model -> Model
handleRedo spec model =
    { model
        | editor =
            Result.withDefault model.editor
                (apply ( "redo", internal <| Command.Redo ) spec model.editor)
    }


handleUndo : Spec -> Model -> Model
handleUndo spec model =
    { model
        | editor =
            Result.withDefault model.editor
                (apply ( "undo", internal <| Command.Undo ) spec model.editor)
    }


handleToggleBlock : Spec -> String -> Model -> Model
handleToggleBlock spec block model =
    let
        isCode =
            block == "Code block"

        onParams =
            if isCode then
                element
                    codeBlock
                    []

            else
                element
                    heading
                    [ IntegerAttribute
                        "level"
                        (Maybe.withDefault 1 <| String.toInt (String.right 1 block))
                    ]

        offParams =
            element paragraph []
    in
    { model
        | editor =
            Result.withDefault model.editor
                (apply
                    ( "toggleBlock"
                    , transform <| toggleTextBlock onParams offParams isCode
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
                (apply
                    ( "wrapBlockquote"
                    , transform <|
                        wrap
                            (\n -> n)
                            (element blockquote [])
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
                (apply
                    ( "insertHR"
                    , transform <|
                        insertBlock
                            (block
                                (element
                                    horizontalRule
                                    []
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


view : Config Msg -> Model -> Html Msg
view cfg model =
    div [ Html.Attributes.class "editor-container" ]
        [ Controls.editorControlPanel model.styles model.editor
        , Editor.view cfg model.editor
        , Controls.renderInsertLinkModal model.insertLinkModal
        , Controls.renderInsertImageModal model.insertImageModal
        ]
