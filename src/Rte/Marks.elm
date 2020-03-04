module Rte.Marks exposing (..)

import Array exposing (Array)
import List.Extra
import Rte.Model exposing (EditorBlockNode, EditorInlineLeaf(..), EditorNode(..), InlineLeafTree(..), Mark, NodePath)
import Rte.Node exposing (map, nodeAt, replace)


hasMarkWithName : String -> List Mark -> Bool
hasMarkWithName name marks =
    List.any (\m -> m.name == name) marks


toggleMarkAtPath : ToggleAction -> Mark -> NodePath -> EditorBlockNode -> Result String EditorBlockNode
toggleMarkAtPath action mark path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (toggleMark action mark n) node


type ToggleAction
    = Add
    | Remove
    | Flip


toggle : ToggleAction -> Mark -> List Mark -> List Mark
toggle toggleAction mark marks =
    let
        isMember =
            List.any (\m -> m.name == mark.name) marks
    in
    if toggleAction == Remove || (toggleAction == Flip && isMember) then
        List.filter (\x -> x.name /= mark.name) marks

    else if not isMember then
        List.sortBy (\m -> m.name) (mark :: marks)

    else
        marks


clearMarks : Mark -> EditorBlockNode -> EditorBlockNode
clearMarks mark root =
    case map (removeMark mark) (BlockNodeWrapper root) of
        BlockNodeWrapper bn ->
            bn

        _ ->
            root


removeMark : Mark -> EditorNode -> EditorNode
removeMark =
    toggleMark Remove


addMark : Mark -> EditorNode -> EditorNode
addMark =
    toggleMark Add


toggleMark : ToggleAction -> Mark -> EditorNode -> EditorNode
toggleMark action mark node =
    case node of
        BlockNodeWrapper _ ->
            node

        InlineLeafWrapper il ->
            InlineLeafWrapper <|
                case il of
                    TextLeaf leaf ->
                        TextLeaf { leaf | marks = toggle action mark leaf.marks }

                    InlineLeaf leaf ->
                        InlineLeaf { leaf | marks = toggle action mark leaf.marks }
