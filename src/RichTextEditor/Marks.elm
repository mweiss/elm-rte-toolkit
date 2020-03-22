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
        ( Block
        , Inline(..)
        , Path
        )
import RichTextEditor.Model.Text as Text
import RichTextEditor.Node exposing (Node(..), nodeAt, replace)


hasMarkWithName : String -> List Mark -> Bool
hasMarkWithName name marks =
    List.any (\m -> name == Mark.name m) marks


toggleMarkAtPath : ToggleAction -> MarkOrder -> Mark -> Path -> Block -> Result String Block
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
                    Text leaf ->
                        Text <|
                            (leaf
                                |> Text.withMarks
                                    (toggle action markOrder mark (Text.marks leaf))
                            )

                    InlineElement leaf ->
                        InlineElement <|
                            (leaf
                                |> InlineElement.withMarks
                                    (toggle action markOrder mark (InlineElement.marks leaf))
                            )
