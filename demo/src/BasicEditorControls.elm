module BasicEditorControls exposing (..)

import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (preventDefaultOn)
import Json.Decode exposing (succeed)
import Rte.List exposing (ListType(..))
import Rte.Model exposing (EditorState, InternalEditorMsg)


type alias InsertLinkModal =
    { visible : Bool
    , editorState : Maybe EditorState
    , href : String
    , title : String
    }


type alias InsertImageModal =
    { visible : Bool
    , editorState : Maybe EditorState
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


createButtonForStyle : List String -> String -> Html EditorMsg
createButtonForStyle currentStyles action =
    let
        selected =
            isCurrentStyle action currentStyles
    in
    createButton selected (onButtonPressToggleStyle action) action


createButton : Bool -> Html.Attribute EditorMsg -> String -> Html EditorMsg
createButton selected actionAttribute action =
    span
        ([ actionAttribute, class "rte-button" ]
            ++ (if selected then
                    [ class "rte-button-selected" ]

                else
                    []
               )
        )
        [ text action ]


inlineElementButtons : List (Html EditorMsg)
inlineElementButtons =
    [ createButton False onButtonPressInsertCode "Code", createButton False onButtonPressInsertLink "Link", createButton False onButtonPressInsertImage "Image" ]


blockElements : List (Html EditorMsg)
blockElements =
    [ createButton False (onButtonPressToggleList Ordered) "OL"
    , createButton False (onButtonPressToggleList Unordered) "UL"
    , createButton False onButtonPressInsertHR "HR"
    , createButton False onButtonPressWrapBlockquote "Blockquote"
    , createButton False onButtonPressLiftOutOfBlock "Lift"
    ]


headerElements : List (Html EditorMsg)
headerElements =
    List.map (\block -> createButton False (onButtonPressToggleBlock block) block) [ "H1", "H2", "H3", "H4", "H5", "H6", "Code block" ]


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
            (List.map
                (createButtonForStyle styles)
                [ "Bold", "Italic", "Strikethrough", "Underline" ]
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
