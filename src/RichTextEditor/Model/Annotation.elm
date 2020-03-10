module RichTextEditor.Model.Annotation exposing
    ( Annotation
    , selectableAnnotation
    , selectionAnnotation
    )


type alias Annotation =
    String


selectionAnnotation : Annotation
selectionAnnotation =
    "__selection__"


selectableAnnotation : Annotation
selectableAnnotation =
    "__selectable__"
