module Rte.Marks exposing (..)

import Rte.Model exposing (EditorBlockNode, EditorInlineLeaf(..), Mark, NodePath)
import Rte.Node exposing (EditorNode(..), nodeAt, replace)


findMarksFromInlineLeaf : EditorInlineLeaf -> List Mark
findMarksFromInlineLeaf leaf =
    case leaf of
        TextLeaf l ->
            l.marks

        InlineLeaf l ->
            l.marks


toggleMarkAtPath : ToggleAction -> Mark -> NodePath -> EditorBlockNode -> Result String EditorBlockNode
toggleMarkAtPath action mark path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            case n of
                BlockNodeWrapper blockNode ->
                    let
                        parameters =
                            blockNode.parameters

                        newBlock =
                            { blockNode | parameters = { parameters | marks = toggleMark action mark parameters.marks } }
                    in
                    replace path (BlockNodeWrapper newBlock) node

                InlineLeafWrapper inlineLeaf ->
                    case inlineLeaf of
                        TextLeaf l ->
                            let
                                newLeaf =
                                    { l | marks = toggleMark action mark l.marks }
                            in
                            replace path (InlineLeafWrapper (TextLeaf newLeaf)) node

                        InlineLeaf l ->
                            let
                                newLeaf =
                                    { l | marks = toggleMark action mark l.marks }
                            in
                            replace path (InlineLeafWrapper (InlineLeaf newLeaf)) node


type ToggleAction
    = Add
    | Remove
    | Flip


toggleMark : ToggleAction -> Mark -> List Mark -> List Mark
toggleMark toggleAction mark marks =
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
