module RichTextEditor.Decorations exposing (selectableDecoration)

import Html
import Html.Attributes
import Html.Events
import RichTextEditor.Model.Annotations exposing (selection)
import RichTextEditor.Model.Editor
    exposing
        ( InternalEditorMsg(..)
        , Tagger
        )
import RichTextEditor.Model.Node
    exposing
        ( Element
        , Path
        , annotationsFromElementParameters
        )
import RichTextEditor.Model.Selection exposing (caretSelection)
import Set


selectableDecoration : Tagger msg -> Path -> Element -> Path -> List (Html.Attribute msg)
selectableDecoration tagger editorNodePath elementParameters _ =
    (if Set.member selection (annotationsFromElementParameters elementParameters) then
        [ Html.Attributes.class "rte-selected" ]

     else
        []
    )
        ++ [ Html.Events.onClick
                (tagger <|
                    SelectionEvent (Just (caretSelection editorNodePath 0)) False
                )
           ]
