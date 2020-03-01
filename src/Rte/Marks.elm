module Rte.Marks exposing (..)

import List.Extra
import Rte.Model exposing (EditorBlockNode, EditorInlineLeaf(..), Mark, NodePath)
import Rte.Node exposing (EditorNode(..), map, nodeAt, replace)


type alias NodeContents =
    { mark : Mark, start : Int, end : Int }


type alias LeafContents =
    { start : Int, end : Int }


type MarkNode
    = Node NodeContents (List MarkNode)
    | Leaf LeafContents


marksToMarkNodeList : List (List Mark) -> List MarkNode
marksToMarkNodeList =
    marksToMarkNodeListRec 0


marksToMarkNodeListRec : Int -> List (List Mark) -> List MarkNode
marksToMarkNodeListRec offset markLists =
    Tuple.second <|
        List.foldr
            (\( ( firstMark, restMarks ), restMarkLists ) ( i, agg ) ->
                let
                    length =
                        List.length restMarkLists + 1
                in
                case firstMark of
                    Nothing ->
                        ( i - length, Leaf { start = i - length, end = i - 1 } :: agg )

                    Just mark ->
                        ( i - length
                        , (Node { mark = mark, start = i - length, end = i - 1 } <| marksToMarkNodeListRec (i - length) (restMarks :: List.map Tuple.second restMarkLists)) :: agg
                        )
            )
            ( offset + List.length markLists, [] )
        <|
            List.Extra.groupWhile
                (\( m1, _ ) ( m2, _ ) -> m1 == m2)
            <|
                List.map (\a -> ( List.head a, List.drop 1 a )) markLists


findMarksFromInlineLeaf : EditorInlineLeaf -> List Mark
findMarksFromInlineLeaf leaf =
    case leaf of
        TextLeaf l ->
            l.marks

        InlineLeaf l ->
            l.marks


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
