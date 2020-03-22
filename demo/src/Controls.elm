module Controls exposing (..)

import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Solid
import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Html.Events exposing (preventDefaultOn)
import Json.Decode exposing (succeed)
import RichTextEditor.List exposing (ListType(..))
import RichTextEditor.Model.Editor exposing (Editor, InternalEditorMsg, state)
import RichTextEditor.Model.Mark as Mark
import RichTextEditor.Model.Node
    exposing
        ( Path
        , elementFromBlockNode
        , marksFromInlineLeaf
        , nameFromElement
        )
import RichTextEditor.Model.Selection exposing (anchorNode, focusNode, normalize)
import RichTextEditor.Model.State as State exposing (State)
import RichTextEditor.Node as Node exposing (Node(..))
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
    = InternalMsg InternalEditorMsg
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


statusForStyle : Style -> ControlState -> Status
statusForStyle style controlState =
    if not controlState.hasInline then
        Disabled

    else if Set.member (styleToString style) controlState.marks then
        Active

    else
        Enabled


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
    preventDefaultOn "click" (succeed ( ToggleStyle style, True ))


onButtonPressToggleList : ListType -> Attribute EditorMsg
onButtonPressToggleList listType =
    preventDefaultOn "click" (succeed ( WrapInList listType, True ))


onButtonPressToggleBlock : String -> Attribute EditorMsg
onButtonPressToggleBlock action =
    preventDefaultOn "click" (succeed ( ToggleBlock action, True ))


onButtonPressWrapBlockquote : Attribute EditorMsg
onButtonPressWrapBlockquote =
    preventDefaultOn "click" (succeed ( WrapInBlockQuote, True ))


onButtonPressInsertLink : Attribute EditorMsg
onButtonPressInsertLink =
    preventDefaultOn "click" (succeed ( ShowInsertLinkModal, True ))


onButtonPressInsertImage : Attribute EditorMsg
onButtonPressInsertImage =
    preventDefaultOn "click" (succeed ( ShowInsertImageModal, True ))


onButtonPressInsertCode : Attribute EditorMsg
onButtonPressInsertCode =
    preventDefaultOn "click" (succeed ( ToggleStyle Code, True ))


onButtonPressLiftOutOfBlock : Attribute EditorMsg
onButtonPressLiftOutOfBlock =
    preventDefaultOn "click" (succeed ( LiftOutOfBlock, True ))


onButtonPressInsertHR : Attribute EditorMsg
onButtonPressInsertHR =
    preventDefaultOn "click" (succeed ( InsertHorizontalRule, True ))


createButtonForStyle : ControlState -> Style -> Icon -> Html EditorMsg
createButtonForStyle controlState style icon =
    let
        status =
            statusForStyle style controlState
    in
    createButton status (onButtonPressToggleStyle style) icon


createButton : Status -> Html.Attribute EditorMsg -> Icon -> Html EditorMsg
createButton status actionAttribute icon =
    span
        ([ actionAttribute, class "rte-button" ]
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
    [ createButton codeStatus onButtonPressInsertCode Solid.code
    , createButton linkStatus onButtonPressInsertLink Solid.link
    , createButton imageStatus onButtonPressInsertImage Solid.image
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
    [ createButton blockStatus (onButtonPressToggleList Ordered) Solid.listOl
    , createButton blockStatus (onButtonPressToggleList Unordered) Solid.listUl
    , createButton blockStatus onButtonPressInsertHR Solid.minus
    , createButton blockStatus onButtonPressWrapBlockquote Solid.quoteRight
    , createButton liftStatus onButtonPressLiftOutOfBlock Solid.outdent
    ]


headerElements : ControlState -> List (Html EditorMsg)
headerElements controlState =
    List.map2
        (\block icon ->
            createButton
                (if controlState.hasInline then
                    Enabled

                 else
                    Disabled
                )
                (onButtonPressToggleBlock block)
                icon
        )
        [ "H1", "Code block" ]
        [ Solid.heading, Solid.codeBranch ]


type alias ControlState =
    { hasInline : Bool
    , hasSelection : Bool
    , nodes : Set String
    , marks : Set String
    , canLift : Bool
    }


emptyControlState : ControlState
emptyControlState =
    { hasInline = False, hasSelection = False, nodes = Set.empty, marks = Set.empty, canLift = False }


accumulateControlState : Node -> ControlState -> ControlState
accumulateControlState node controlState =
    case node of
        Block n ->
            { controlState
                | nodes =
                    Set.insert (nameFromElement (elementFromBlockNode n)) controlState.nodes
            }

        Inline inline ->
            let
                names =
                    List.map Mark.name (marksFromInlineLeaf inline)
            in
            { controlState | hasInline = True, marks = Set.union (Set.fromList names) controlState.marks }


deriveControlState : State -> ControlState
deriveControlState state =
    case State.selection state of
        Nothing ->
            emptyControlState

        Just selection ->
            let
                normalizedSelection =
                    normalize selection

                controlState =
                    Node.foldlRange (anchorNode normalizedSelection)
                        (focusNode normalizedSelection)
                        accumulateControlState
                        { emptyControlState | hasSelection = True }
                        (State.root state)
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
            deriveControlState (state editor)
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
