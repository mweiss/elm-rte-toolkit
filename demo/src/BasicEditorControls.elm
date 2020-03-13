module BasicEditorControls exposing (..)

import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (preventDefaultOn)
import Json.Decode exposing (succeed)
import RichTextEditor.List exposing (ListType(..))
import RichTextEditor.Model.Editor exposing (InternalEditorMsg)
import RichTextEditor.Model.State exposing (State)


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


type EditorMsg
    = InternalMsg InternalEditorMsg
    | ToggleStyle String
    | ShowInsertLinkModal
    | UpdateLinkHref String
    | UpdateLinkTitle String
    | InsertLink
    | InsertCode
    | ToggleBlock String
    | WrapInList ListType
    | ShowInsertImageModal
    | InsertImage
    | UpdateImageSrc String
    | UpdateImageAlt String
    | InsertHorizontalRule
    | WrapInBlockQuote
    | JoinPreviousBlock
    | LiftOutOfBlock


isCurrentStyle : String -> List String -> Bool
isCurrentStyle action currentStyles =
    List.member action currentStyles


onButtonPressToggleStyle : String -> Attribute EditorMsg
onButtonPressToggleStyle action =
    preventDefaultOn "click" (succeed ( ToggleStyle action, True ))


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
    preventDefaultOn "click" (succeed ( InsertCode, True ))


onButtonPressLiftOutOfBlock : Attribute EditorMsg
onButtonPressLiftOutOfBlock =
    preventDefaultOn "click" (succeed ( LiftOutOfBlock, True ))


onButtonPressInsertHR : Attribute EditorMsg
onButtonPressInsertHR =
    preventDefaultOn "click" (succeed ( InsertHorizontalRule, True ))


createButtonForStyle : List String -> String -> Icon -> Html EditorMsg
createButtonForStyle currentStyles action icon =
    let
        selected =
            isCurrentStyle action currentStyles
    in
    createButton selected (onButtonPressToggleStyle action) icon


createButton : Bool -> Html.Attribute EditorMsg -> Icon -> Html EditorMsg
createButton selected actionAttribute icon =
    span
        ([ actionAttribute, class "rte-button" ]
            ++ (if selected then
                    [ class "rte-button-selected" ]

                else
                    []
               )
        )
        [ Icon.viewIcon icon ]


inlineElementButtons : List (Html EditorMsg)
inlineElementButtons =
    [ createButton False onButtonPressInsertCode Icon.code, createButton False onButtonPressInsertLink Icon.link, createButton False onButtonPressInsertImage Icon.image ]


blockElements : List (Html EditorMsg)
blockElements =
    [ createButton False (onButtonPressToggleList Ordered) Icon.listOl
    , createButton False (onButtonPressToggleList Unordered) Icon.listUl
    , createButton False onButtonPressInsertHR Icon.gripLines
    , createButton False onButtonPressWrapBlockquote Icon.quoteRight
    , createButton False onButtonPressLiftOutOfBlock Icon.outdent
    ]


headerElements : List (Html EditorMsg)
headerElements =
    List.map2
        (\block icon ->
            createButton False (onButtonPressToggleBlock block) icon
        )
        [ "H1", "Code block" ]
        [ Icon.heading, Icon.codeBranch ]


editorControlPanel : List String -> Html EditorMsg
editorControlPanel styles =
    div [ class "rte-controls-container" ]
        [ {- div [ class "rte-controls" ]
                 (List.map
                     (createButton document True)
                     [ "H1", "H2", "H3", "H4", "H5", "H6", "Blockquote" ]
                 )
             ,
          -}
          div [ class "rte-controls" ]
            (List.map2
                (createButtonForStyle styles)
                [ "Bold", "Italic" ]
                [ Icon.bold, Icon.italic ]
            )
        , div
            [ class "rte-controls" ]
            (inlineElementButtons
                ++ blockElements
            )
        , div
            [ class "rte-controls" ]
            headerElements
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
