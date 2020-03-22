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
import RichTextEditor.Model.Element exposing (Element, annotations)
import RichTextEditor.Model.Node
    exposing
        ( Path
        )
import RichTextEditor.Model.Selection exposing (caretSelection)
import Set


selectableDecoration : Tagger msg -> Path -> Element -> Path -> List (Html.Attribute msg)
selectableDecoration tagger editorNodePath elementParameters _ =
    (if Set.member selection (annotations elementParameters) then
        [ Html.Attributes.class "rte-selected" ]

     else
        []
    )
        ++ [ Html.Events.onClick
                (tagger <|
                    SelectionEvent (Just (caretSelection editorNodePath 0)) False
                )
           ]
