module Controls exposing (..)

import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Solid
import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Html.Events exposing (preventDefaultOn)
import Json.Decode exposing (succeed)
import RichText.Editor exposing (Editor, Message, history, state)
import RichText.List exposing (ListType(..))
import RichText.Model.Element as Element
import RichText.Model.History exposing (peek, redoList)
import RichText.Model.Mark as Mark
import RichText.Model.Node exposing (Block, Path, element, marks, parent)
import RichText.Model.Selection exposing (anchorNode, focusNode, normalize)
import RichText.Model.State as State exposing (State)
import RichText.Node as Node exposing (Node(..))
import Set exposing (Set)


type Status
    = Active
    | Enabled
    | Disabled


type alias InsertLinkModal =
    { visible : Bool
    , editorState : Maybe State
    , href : String
    , title : String
    }


type alias InsertImageModal =
    { visible : Bool
    , editorState : Maybe State
    , src : String
    , alt : String
    }


type Style
    = Bold
    | Italic
    | Code
    | Strikethrough
    | Underline


type EditorMsg
    = InternalMsg Message
    | ToggleStyle Style
    | ShowInsertLinkModal
    | UpdateLinkHref String
    | UpdateLinkTitle String
    | InsertLink
    | ToggleBlock String
    | WrapInList ListType
    | ShowInsertImageModal
    | InsertImage
    | UpdateImageSrc String
    | UpdateImageAlt String
    | InsertHorizontalRule
    | WrapInBlockQuote
    | LiftOutOfBlock
    | Noop
    | CaptionedImage Path String
    | Undo
    | Redo


statusForStyle : Style -> ControlState -> Status
statusForStyle style controlState =
    if not controlState.hasInline || Set.member "code_block" controlState.nodes then
        Disabled

    else if Set.member (styleToString style) controlState.marks then
        Active

    else
        Enabled


titleForStyle : Style -> String
titleForStyle style =
    case style of
        Bold ->
            "bold"

        Italic ->
            "italic"

        Code ->
            "code"

        Underline ->
            "underline"

        Strikethrough ->
            "strikethrough"


styleToString : Style -> String
styleToString style =
    case style of
        Bold ->
            "bold"

        Italic ->
            "italic"

        Code ->
            "code"

        Strikethrough ->
            "strikethrough"

        Underline ->
            "underline"


onButtonPressToggleStyle : Style -> Attribute EditorMsg
onButtonPressToggleStyle style =
    preventDefaultOn "mousedown" (succeed ( ToggleStyle style, True ))


onButtonPressToggleList : ListType -> Attribute EditorMsg
onButtonPressToggleList listType =
    preventDefaultOn "mousedown" (succeed ( WrapInList listType, True ))


onButtonPressToggleBlock : String -> Attribute EditorMsg
onButtonPressToggleBlock action =
    preventDefaultOn "mousedown" (succeed ( ToggleBlock action, True ))


onButtonPressWrapBlockquote : Attribute EditorMsg
onButtonPressWrapBlockquote =
    preventDefaultOn "mousedown" (succeed ( WrapInBlockQuote, True ))


onButtonPressInsertLink : Attribute EditorMsg
onButtonPressInsertLink =
    preventDefaultOn "mousedown" (succeed ( ShowInsertLinkModal, True ))


onButtonPressInsertImage : Attribute EditorMsg
onButtonPressInsertImage =
    preventDefaultOn "mousedown" (succeed ( ShowInsertImageModal, True ))


onButtonPressInsertCode : Attribute EditorMsg
onButtonPressInsertCode =
    preventDefaultOn "mousedown" (succeed ( ToggleStyle Code, True ))


onButtonPressLiftOutOfBlock : Attribute EditorMsg
onButtonPressLiftOutOfBlock =
    preventDefaultOn "mousedown" (succeed ( LiftOutOfBlock, True ))


onButtonPressInsertHR : Attribute EditorMsg
onButtonPressInsertHR =
    preventDefaultOn "mousedown" (succeed ( InsertHorizontalRule, True ))


createButtonForStyle : ControlState -> Style -> Icon -> Html EditorMsg
createButtonForStyle controlState style icon =
    let
        status =
            statusForStyle style controlState

        title =
            titleForStyle style
    in
    createButton status (onButtonPressToggleStyle style) icon title


createButton : Status -> Html.Attribute EditorMsg -> Icon -> String -> Html EditorMsg
createButton status actionAttribute icon title =
    span
        ([ actionAttribute, Html.Attributes.title title, class "rte-button" ]
            ++ (case status of
                    Active ->
                        [ class "rte-active" ]

                    Disabled ->
                        [ class "rte-disabled" ]

                    Enabled ->
                        [ class "rte-enabled" ]
               )
        )
        [ Icon.viewIcon icon ]


inlineElementButtons : ControlState -> List (Html EditorMsg)
inlineElementButtons controlState =
    let
        codeStatus =
            if not controlState.hasInline then
                Disabled

            else if Set.member "code" controlState.marks then
                Active

            else
                Enabled

        linkStatus =
            if not controlState.hasInline then
                Disabled

            else if Set.member "link" controlState.marks then
                Active

            else
                Enabled

        imageStatus =
            if not controlState.hasInline then
                Disabled

            else
                Enabled
    in
    [ createButton codeStatus onButtonPressInsertCode Solid.code "code"
    , createButton linkStatus onButtonPressInsertLink Solid.link "link"
    , createButton imageStatus onButtonPressInsertImage Solid.image "image"
    ]


blockElements : ControlState -> List (Html EditorMsg)
blockElements controlStatus =
    let
        blockStatus =
            if controlStatus.hasSelection then
                Enabled

            else
                Disabled

        liftStatus =
            if controlStatus.canLift then
                Enabled

            else
                Disabled
    in
    [ createButton blockStatus (onButtonPressToggleList Ordered) Solid.listOl "ordered list"
    , createButton blockStatus (onButtonPressToggleList Unordered) Solid.listUl "unordered list"
    , createButton blockStatus onButtonPressInsertHR Solid.minus "horizontal rule"
    , createButton blockStatus onButtonPressWrapBlockquote Solid.quoteRight "blockquote"
    , createButton liftStatus onButtonPressLiftOutOfBlock Solid.outdent "lift"
    ]


headerElements : ControlState -> List (Html EditorMsg)
headerElements controlState =
    List.map3
        (\block icon title ->
            createButton
                (if controlState.hasInline then
                    if Set.member (String.replace " " "_" title) controlState.nodes then
                        Active

                    else
                        Enabled

                 else
                    Disabled
                )
                (onButtonPressToggleBlock block)
                icon
                title
        )
        [ "H1", "Code block" ]
        [ Solid.heading, Solid.codeBranch ]
        [ "heading", "code block" ]


type alias ControlState =
    { hasInline : Bool
    , hasSelection : Bool
    , hasUndo : Bool
    , hasRedo : Bool
    , nodes : Set String
    , marks : Set String
    , canLift : Bool
    }


emptyControlState : ControlState
emptyControlState =
    { hasUndo = False, hasRedo = False, hasInline = False, hasSelection = False, nodes = Set.empty, marks = Set.empty, canLift = False }


accumulateControlState : Node -> ControlState -> ControlState
accumulateControlState node controlState =
    case node of
        Block n ->
            { controlState
                | nodes =
                    Set.insert (Element.name (element n)) controlState.nodes
            }

        Inline inline ->
            let
                names =
                    List.map Mark.name (marks inline)
            in
            { controlState | hasInline = True, marks = Set.union (Set.fromList names) controlState.marks }


accumulateControlStateWithRanges : List ( Path, Path ) -> Block -> ControlState -> ControlState
accumulateControlStateWithRanges ranges root controlState =
    List.foldl
        (\( start, end ) cs ->
            Node.foldlRange start
                end
                accumulateControlState
                cs
                root
        )
        controlState
        ranges


deriveControlState : Editor -> ControlState
deriveControlState editor =
    let
        state_ =
            state editor

        history_ =
            history editor
    in
    case State.selection state_ of
        Nothing ->
            emptyControlState

        Just selection ->
            let
                hasUndo =
                    peek history_ /= Nothing

                hasRedo =
                    not <| List.isEmpty (redoList history_)

                normalizedSelection =
                    normalize selection

                parentFocus =
                    parent (focusNode normalizedSelection)

                parentAnchor =
                    parent (anchorNode normalizedSelection)

                controlState =
                    accumulateControlStateWithRanges
                        [ ( anchorNode normalizedSelection, focusNode normalizedSelection )
                        , ( parentFocus, parentFocus )
                        , ( parentAnchor, parentAnchor )
                        ]
                        (State.root state_)
                        { emptyControlState | hasSelection = True }
            in
            { controlState
                | canLift =
                    -- This is hacky, but we'll assume we can lift anything that's nested
                    -- three or more nodes deep.
                    List.length (anchorNode normalizedSelection)
                        > 2
                        || List.length (focusNode normalizedSelection)
                        > 2
                        || Set.member "blockquote" controlState.nodes
                        || Set.member "li" controlState.nodes
                , hasUndo = hasUndo
                , hasRedo = hasRedo
            }


styleToIcon : Style -> Icon
styleToIcon style =
    case style of
        Bold ->
            Solid.bold

        Italic ->
            Solid.italic

        Code ->
            Solid.code

        Strikethrough ->
            Solid.strikethrough

        Underline ->
            Solid.underline


editorControlPanel : List Style -> Editor -> Html EditorMsg
editorControlPanel styles editor =
    let
        controlState =
            deriveControlState editor
    in
    div [ class "rte-controls-container" ]
        [ div [ class "rte-controls" ]
            (List.map2
                (createButtonForStyle controlState)
                styles
                (List.map styleToIcon styles)
            )
        , div
            [ class "rte-controls" ]
            (inlineElementButtons controlState
                ++ blockElements controlState
            )
        , div
            [ class "rte-controls" ]
          <|
            headerElements controlState
        , div
            [ class "rte-controls" ]
          <|
            undoRedo controlState
        ]


undoRedo : ControlState -> List (Html EditorMsg)
undoRedo controlState =
    [ createButton
        (if controlState.hasUndo then
            Enabled

         else
            Disabled
        )
        (preventDefaultOn "mousedown" (succeed ( Undo, True )))
        Solid.undo
        "undo"
    , createButton
        (if controlState.hasRedo then
            Enabled

         else
            Disabled
        )
        (preventDefaultOn "mousedown" (succeed ( Redo, True )))
        Solid.redo
        "redo"
    ]


modal : Bool -> List (Html msg) -> Html msg
modal visible children =
    Html.div [ Html.Attributes.classList [ ( "modal", True ), ( "modal-isOpen", visible ), ( "modal--top", True ) ] ]
        [ Html.div [ Html.Attributes.class "modal-container" ] children
        , Html.div [ Html.Attributes.class "modal-backdrop" ] []
        ]


renderInsertImageModal : InsertImageModal -> Html EditorMsg
renderInsertImageModal insertImageModal =
    modal insertImageModal.visible
        [ Html.h3 []
            [ Html.text "Insert image" ]
        , Html.div
            []
            [ Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.name "src"
                , Html.Attributes.value insertImageModal.src
                , Html.Attributes.placeholder "Image URL (ex: https://via.placeholder.com/150.png)"
                , Html.Events.onInput UpdateImageSrc
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
                , Html.Events.onInput UpdateImageAlt
                ]
                []
            ]
        , Html.div
            []
            [ Html.button
                [ Html.Events.onClick InsertImage ]
                [ Html.text "Insert" ]
            ]
        ]


renderInsertLinkModal : InsertLinkModal -> Html EditorMsg
renderInsertLinkModal insertLinkModal =
    modal insertLinkModal.visible
        [ Html.h3 []
            [ Html.text "Insert link" ]
        , Html.div
            []
            [ Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.name "href"
                , Html.Attributes.value insertLinkModal.href
                , Html.Attributes.placeholder "Location"
                , Html.Events.onInput UpdateLinkHref
                ]
                []
            ]
        , Html.div
            []
            [ Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.name "title"
                , Html.Attributes.value insertLinkModal.title
                , Html.Attributes.placeholder "Title"
                , Html.Events.onInput UpdateLinkTitle
                ]
                []
            ]
        , Html.div
            []
            [ Html.button
                [ Html.Events.onClick InsertLink ]
                [ Html.text "Insert" ]
            ]
        ]
