module RichTextEditor.Marks exposing
    ( addMark
    , hasMarkWithName
    , removeMark
    , toggleMark
    , toggleMarkAtPath
    )

import RichTextEditor.Model.InlineElement as InlineElement
import RichTextEditor.Model.Mark as Mark exposing (Mark, MarkOrder, ToggleAction(..), name, toggle)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , InlineLeaf(..)
        , Path
        )
import RichTextEditor.Model.Text as Text
import RichTextEditor.Node exposing (Node(..), nodeAt, replace)


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
        Block _ ->
            node

        Inline il ->
            Inline <|
                case il of
                    TextLeaf leaf ->
                        TextLeaf <|
                            (leaf
                                |> Text.withMarks
                                    (toggle action markOrder mark (Text.marks leaf))
                            )

                    ElementLeaf leaf ->
                        ElementLeaf <|
                            (leaf
                                |> InlineElement.withMarks
                                    (toggle action markOrder mark (InlineElement.marks leaf))
                            )
