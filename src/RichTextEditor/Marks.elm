module RichTextEditor.Marks exposing (..)

import Dict
import RichTextEditor.Model
    exposing
        ( EditorBlockNode
        , EditorInlineLeaf(..)
        , EditorNode(..)
        , InlineLeafTree(..)
        , Mark
        , MarkOrder
        , NodePath
        )
import RichTextEditor.Node exposing (nodeAt, replace)


hasMarkWithName : String -> List Mark -> Bool
hasMarkWithName name marks =
    List.any (\m -> m.name == name) marks


toggleMarkAtPath : ToggleAction -> MarkOrder -> Mark -> NodePath -> EditorBlockNode -> Result String EditorBlockNode
toggleMarkAtPath action markOrder mark path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (toggleMark action markOrder mark n) node


type ToggleAction
    = Add
    | Remove
    | Flip


toggle : ToggleAction -> MarkOrder -> Mark -> List Mark -> List Mark
toggle toggleAction markOrder mark marks =
    let
        isMember =
            List.any (\m -> m.name == mark.name) marks
    in
    if toggleAction == Remove || (toggleAction == Flip && isMember) then
        List.filter (\x -> x.name /= mark.name) marks

    else if not isMember then
        List.sortBy
            (\m -> ( Maybe.withDefault 0 <| Dict.get m.name markOrder, m.name ))
            (mark :: marks)

    else
        marks


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
                        TextLeaf { leaf | marks = toggle action markOrder mark leaf.marks }

                    InlineLeaf leaf ->
                        InlineLeaf { leaf | marks = toggle action markOrder mark leaf.marks }
