module RichTextEditor.Internal.Selection exposing
    ( domToEditor
    , editorToDom
    )

import RichTextEditor.Config.Spec exposing (Spec)
import RichTextEditor.Internal.Path as Path
import RichTextEditor.Model.Node exposing (Block, Path)
import RichTextEditor.Model.Selection
    exposing
        ( Selection
        , anchorNode
        , anchorOffset
        , focusNode
        , focusOffset
        , range
        )


domToEditor : Spec -> Block -> Selection -> Maybe Selection
domToEditor spec =
    transformSelection (Path.domToEditor spec)


editorToDom : Spec -> Block -> Selection -> Maybe Selection
editorToDom spec =
    transformSelection (Path.editorToDom spec)


transformSelection : (Block -> Path -> Maybe Path) -> Block -> Selection -> Maybe Selection
transformSelection transformation node selection =
    case transformation node (anchorNode selection) of
        Nothing ->
            Nothing

        Just an ->
            case transformation node (focusNode selection) of
                Nothing ->
                    Nothing

                Just fn ->
                    Just <| range an (anchorOffset selection) fn (focusOffset selection)
