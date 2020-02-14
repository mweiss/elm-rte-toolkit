module Rte.NodeUtils exposing (EditorNode(..), NodeResult(..), findNode, foldl, removeNodesInRange, replaceNode, replaceNodeWithFragment)

import Array exposing (Array)
import Array.Extra
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf, NodePath)


type NodeResult
    = BlockNodeResult EditorBlockNode
    | InlineLeafResult EditorInlineLeaf
    | NoResult


type EditorNode
    = BlockNodeWrapper EditorBlockNode
    | InlineLeafWrapper EditorInlineLeaf


type EditorFragment
    = BlockNodeFragment (Array EditorBlockNode)
    | InlineLeafFragment (Array EditorInlineLeaf)


foldl : (NodePath -> EditorNode -> b -> b) -> b -> EditorNode -> b
foldl func acc fragment =
    foldlRec func acc fragment []


foldlRec : (NodePath -> EditorNode -> b -> b) -> b -> EditorNode -> NodePath -> b
foldlRec func acc node path =
    case node of
        BlockNodeWrapper blockNode ->
            let
                children =
                    Array.indexedMap Tuple.pair <|
                        case blockNode.childNodes of
                            Leaf ->
                                Array.empty

                            InlineLeafArray a ->
                                Array.map InlineLeafWrapper a

                            BlockArray a ->
                                Array.map BlockNodeWrapper a
            in
            Array.foldl
                (\( index, childNode ) agg ->
                    foldlRec func agg childNode (path ++ [ index ])
                )
                (func path node acc)
                children

        InlineLeafWrapper _ ->
            func path node acc



{- replaceNodeWithFragment replaces the node at the node path with the given fragment -}


replaceNodeWithFragment : NodePath -> EditorFragment -> EditorBlockNode -> Result String EditorBlockNode
replaceNodeWithFragment path fragment root =
    case path of
        [] ->
            Err "I cannot replace a node with a fragment"

        [ x ] ->
            case root.childNodes of
                BlockArray a ->
                    case fragment of
                        BlockNodeFragment blocks ->
                            Ok
                                { root
                                    | childNodes =
                                        BlockArray
                                            (Array.append
                                                (Array.append
                                                    (Array.Extra.sliceUntil x a)
                                                    blocks
                                                )
                                                (Array.Extra.sliceFrom (x + 1) a)
                                            )
                                }

                        InlineLeafFragment _ ->
                            Err "I cannot replace a block fragment with an inline leaf fragment"

                InlineLeafArray a ->
                    case fragment of
                        InlineLeafFragment leaves ->
                            Ok
                                { root
                                    | childNodes =
                                        InlineLeafArray
                                            (Array.append
                                                (Array.append
                                                    (Array.Extra.sliceUntil x a)
                                                    leaves
                                                )
                                                (Array.Extra.sliceFrom (x + 1) a)
                                            )
                                }

                        BlockNodeFragment _ ->
                            Err "I cannot replace an inline fragment with an block fragment"

                Leaf ->
                    Err "Not implemented"

        x :: xs ->
            case root.childNodes of
                BlockArray a ->
                    case Array.get x a of
                        Nothing ->
                            Err "I received an invalid path, I can't find a block node at the given index."

                        Just node ->
                            case replaceNodeWithFragment xs fragment node of
                                Ok n ->
                                    Ok { root | childNodes = BlockArray (Array.set x n a) }

                                Err v ->
                                    Err v

                InlineLeafArray _ ->
                    Err "I received an invalid path, I reached an inline leaf array but I still have more path left."

                Leaf ->
                    Err "I received an invalid path, I am on a leaf node, but I still have more path left."



{- replaceNode replaces the node at the nodepath with the given editor node -}


replaceNode : NodePath -> EditorNode -> EditorBlockNode -> Result String EditorBlockNode
replaceNode path node root =
    case path of
        [] ->
            case node of
                BlockNodeWrapper n ->
                    Ok n

                InlineLeafWrapper _ ->
                    Err "I cannot replace a block node with an inline leaf."

        _ ->
            let
                fragment =
                    case node of
                        BlockNodeWrapper n ->
                            BlockNodeFragment <| Array.fromList [ n ]

                        InlineLeafWrapper n ->
                            InlineLeafFragment <| Array.fromList [ n ]
            in
            replaceNodeWithFragment path fragment root


{-| findNode returns the node at the specified NodePath if it exists.
-}
findNode : NodePath -> EditorBlockNode -> NodeResult
findNode path node =
    case path of
        [] ->
            BlockNodeResult node

        x :: xs ->
            case node.childNodes of
                BlockArray list ->
                    case Array.get x list of
                        Nothing ->
                            NoResult

                        Just childNode ->
                            findNode xs childNode

                InlineLeafArray list ->
                    case Array.get x list of
                        Nothing ->
                            NoResult

                        Just childLeafNode ->
                            if List.isEmpty xs then
                                InlineLeafResult childLeafNode

                            else
                                NoResult

                Leaf ->
                    NoResult



{- This method removes all the nodes inclusive to both the start and end node path.  Note that
   an ancestor is not removed if the start path or end path is a child node.
-}


removeNodesInRange : NodePath -> NodePath -> EditorBlockNode -> EditorBlockNode
removeNodesInRange start end node =
    let
        startIndex =
            Maybe.withDefault 0 (List.head start)

        startRest =
            Maybe.withDefault [] (List.tail start)

        endIndex =
            Maybe.withDefault
                (case node.childNodes of
                    BlockArray a ->
                        Array.length a

                    InlineLeafArray a ->
                        Array.length a

                    Leaf ->
                        0
                )
                (List.head end)

        endRest =
            Maybe.withDefault [] (List.tail end)
    in
    if startIndex > endIndex then
        node

    else if startIndex == endIndex then
        case node.childNodes of
            BlockArray a ->
                if List.isEmpty startRest && List.isEmpty endRest then
                    { node | childNodes = BlockArray <| Array.Extra.removeAt startIndex a }

                else
                    case Array.get startIndex a of
                        Nothing ->
                            node

                        Just b ->
                            { node | childNodes = BlockArray <| Array.set startIndex (removeNodesInRange startRest endRest b) a }

            InlineLeafArray a ->
                if List.isEmpty startRest && List.isEmpty endRest then
                    { node | childNodes = InlineLeafArray <| Array.Extra.removeAt startIndex a }

                else
                    node

            Leaf ->
                node

    else
        case node.childNodes of
            BlockArray a ->
                let
                    left =
                        Array.Extra.sliceUntil startIndex a

                    right =
                        Array.Extra.sliceFrom (endIndex + 1) a

                    leftRest =
                        if List.isEmpty startRest then
                            Array.empty

                        else
                            case Array.get startIndex a of
                                Nothing ->
                                    Array.empty

                                Just b ->
                                    Array.fromList [ removeNodesInRange startRest endRest b ]

                    rightRest =
                        if List.isEmpty endRest then
                            Array.empty

                        else
                            case Array.get endIndex a of
                                Nothing ->
                                    Array.empty

                                Just b ->
                                    Array.fromList [ removeNodesInRange startRest endRest b ]
                in
                { node | childNodes = BlockArray <| List.foldr Array.append Array.empty [ left, leftRest, rightRest, right ] }

            InlineLeafArray a ->
                let
                    left =
                        Array.Extra.sliceUntil
                            (if List.isEmpty startRest then
                                startIndex

                             else
                                startIndex + 1
                            )
                            a

                    right =
                        Array.Extra.sliceFrom
                            (if List.isEmpty endRest then
                                endIndex + 1

                             else
                                endIndex
                            )
                            a
                in
                { node | childNodes = InlineLeafArray <| Array.append left right }

            Leaf ->
                node
