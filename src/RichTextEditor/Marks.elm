module RichTextEditor.Marks exposing (..)

import RichTextEditor.Model.Mark as Mark exposing (Mark, MarkOrder, ToggleAction(..), name, toggle)
import RichTextEditor.Model.Node
    exposing
        ( EditorBlockNode
        , EditorInlineLeaf(..)
        , EditorNode(..)
        , NodePath
        , inlineLeafParametersWithMarks
        , marksFromInlineLeafParameters
        , marksFromTextLeafParameters
        , textLeafParametersWithMarks
        )
import RichTextEditor.Node exposing (nodeAt, replace)


hasMarkWithName : String -> List Mark -> Bool
hasMarkWithName name marks =
    List.any (\m -> name == Mark.name m) marks


toggleMarkAtPath : ToggleAction -> MarkOrder -> Mark -> NodePath -> EditorBlockNode -> Result String EditorBlockNode
toggleMarkAtPath action markOrder mark path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (toggleMark action markOrder mark n) node


removeMark : MarkOrder -> Mark -> EditorNode -> EditorNode
removeMark =
    toggleMark Remove


addMark : MarkOrder -> Mark -> EditorNode -> EditorNode
addMark =
    toggleMark Add


toggleMark : ToggleAction -> MarkOrder -> Mark -> EditorNode -> EditorNode
toggleMark action markOrder mark node =
    case node of
        BlockNodeWrapper _ ->
            node

        InlineLeafWrapper il ->
            InlineLeafWrapper <|
                case il of
                    TextLeaf leaf ->
                        TextLeaf <|
                            (leaf
                                |> textLeafParametersWithMarks
                                    (toggle action markOrder mark (marksFromTextLeafParameters leaf))
                            )

                    InlineLeaf leaf ->
                        InlineLeaf <|
                            (leaf
                                |> inlineLeafParametersWithMarks
                                    (toggle action markOrder mark (marksFromInlineLeafParameters leaf))
                            )
