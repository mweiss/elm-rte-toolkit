module RichTextEditor.Model.Text exposing
    ( Text
    , annotations
    , comparableMarks
    , empty
    , marks
    , text
    , withAnnotations
    , withMarks
    , withText
    )

{-| TextNodeContents represents the attributes that can be in a text node. The core attributes
are marks and text.
-}

import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.Mark exposing (Mark, attributes, name)
import Set exposing (Set)


type Text
    = Text TextContents


type alias TextContents =
    { marks : List Mark
    , annotations : Set String
    , text : String
    }


empty : Text
empty =
    Text { text = "", marks = [], annotations = Set.empty }


marks : Text -> List Mark
marks parameters =
    case parameters of
        Text c ->
            c.marks


annotations : Text -> Set String
annotations parameters =
    case parameters of
        Text c ->
            c.annotations


text : Text -> String
text parameters =
    case parameters of
        Text c ->
            c.text


withText : String -> Text -> Text
withText s parameters =
    case parameters of
        Text c ->
            Text { c | text = s }


withAnnotations : Set String -> Text -> Text
withAnnotations ann parameters =
    case parameters of
        Text c ->
            Text { c | annotations = ann }


withMarks : List Mark -> Text -> Text
withMarks m parameters =
    case parameters of
        Text c ->
            Text { c | marks = m }


comparableMarks : Text -> List ( String, List Attribute )
comparableMarks parameters =
    List.map (\m -> ( name m, attributes m )) (marks parameters)
