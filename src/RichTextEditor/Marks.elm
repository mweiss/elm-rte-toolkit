module RichTextEditor.Marks exposing (..)

import RichTextEditor.Model.Mark as Mark exposing (Mark, MarkOrder, ToggleAction(..), name, toggle)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , EditorInlineLeaf(..)
        , Node(..)
        , Path
        , inlineLeafParametersWithMarks
        , marksFromInlineLeafParameters
        , marksFromTextLeafParameters
        , textLeafParametersWithMarks
        )
import RichTextEditor.Node exposing (nodeAt, replace)


hasMarkWithName : String -> List Mark -> Bool
hasMarkWithName name marks =
    List.any (\m -> name == Mark.name m) marks


toggleMarkAtPath : ToggleAction -> MarkOrder -> Mark -> Path -> BlockNode -> Result String BlockNode
toggleMarkAtPath action markOrder mark path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (toggleMark action markOrder mark n) node


removeMark : MarkOrder -> Mark -> Node -> Node
removeMark =
    toggleMark Remove


addMark : MarkOrder -> Mark -> Node -> Node
addMark =
    toggleMark Add


toggleMark : ToggleAction -> MarkOrder -> Mark -> Node -> Node
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
