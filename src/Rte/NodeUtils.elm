module Rte.NodeUtils exposing (EditorNode(..), NodeResult(..), findNodeBackwardFrom, findNodeBackwardFromExclusive, findNodeForwardFrom, findNodeForwardFromExclusive, findTextBlockNodeAncestor, foldl, map, nodeAt, removeNodeAndEmptyParents, removeNodesInRange, replaceNode, replaceNodeWithFragment)

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


findLastPath : EditorBlockNode -> ( NodePath, EditorNode )
findLastPath node =
    case node.childNodes of
        BlockArray a ->
            let
                lastIndex =
                    Array.length a - 1
            in
            case Array.get lastIndex a of
                Nothing ->
                    ( [], BlockNodeWrapper node )

                Just b ->
                    let
                        ( p, n ) =
                            findLastPath b
                    in
                    ( lastIndex :: p, n )

        InlineLeafArray a ->
            let
                lastIndex =
                    Array.length a - 1
            in
            case Array.get lastIndex a of
                Nothing ->
                    ( [], BlockNodeWrapper node )

                Just l ->
                    ( [ lastIndex ], InlineLeafWrapper l )

        Leaf ->
            ( [], BlockNodeWrapper node )


type alias Iterator =
    NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )


previous : Iterator
previous path node =
    case path of
        [] ->
            Nothing

        [ x ] ->
            let
                prevIndex =
                    x - 1
            in
            case node.childNodes of
                BlockArray a ->
                    case Array.get prevIndex a of
                        Nothing ->
                            Nothing

                        Just b ->
                            let
                                ( p, n ) =
                                    findLastPath b
                            in
                            Just ( prevIndex :: p, n )

                InlineLeafArray a ->
                    case Array.get prevIndex a of
                        Nothing ->
                            Nothing

                        Just l ->
                            Just ( [ prevIndex ], InlineLeafWrapper l )

                Leaf ->
                    Nothing

        x :: xs ->
            case node.childNodes of
                BlockArray a ->
                    case Array.get x a of
                        Nothing ->
                            Nothing

                        Just b ->
                            case previous xs b of
                                Nothing ->
                                    Just ( [ x ], BlockNodeWrapper b )

                                Just ( p, n ) ->
                                    Just ( x :: p, n )

                InlineLeafArray a ->
                    case Array.get (x - 1) a of
                        Nothing ->
                            Nothing

                        Just l ->
                            Just ( [ x - 1 ], InlineLeafWrapper l )

                Leaf ->
                    Nothing


next : Iterator
next path node =
    case path of
        [] ->
            case node.childNodes of
                BlockArray a ->
                    case Array.get 0 a of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ 0 ], BlockNodeWrapper b )

                InlineLeafArray a ->
                    case Array.get 0 a of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ 0 ], InlineLeafWrapper b )

                Leaf ->
                    Nothing

        x :: xs ->
            case node.childNodes of
                BlockArray a ->
                    case Array.get x a of
                        Nothing ->
                            Nothing

                        Just b ->
                            case next xs b of
                                Nothing ->
                                    case Array.get (x + 1) a of
                                        Nothing ->
                                            Nothing

                                        Just bNext ->
                                            Just ( [ x + 1 ], BlockNodeWrapper bNext )

                                Just ( p, n ) ->
                                    Just ( x :: p, n )

                InlineLeafArray a ->
                    case Array.get (x + 1) a of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ x + 1 ], InlineLeafWrapper b )

                Leaf ->
                    Nothing


findNodeForwardFrom : (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findNodeForwardFrom =
    findNodeFrom next


findNodeForwardFromExclusive : (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findNodeForwardFromExclusive =
    findNodeFromExclusive next


findNodeBackwardFrom : (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findNodeBackwardFrom =
    findNodeFrom previous


findNodeBackwardFromExclusive : (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findNodeBackwardFromExclusive =
    findNodeFromExclusive previous


findNodeFromExclusive : Iterator -> (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findNodeFromExclusive iterator pred path node =
    case iterator path node of
        Nothing ->
            Nothing

        Just ( nextPath, _ ) ->
            findNodeFrom iterator pred nextPath node


findNodeFrom : Iterator -> (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findNodeFrom iterator pred path node =
    case nodeAt path node of
        Just n ->
            if pred path n then
                Just ( path, n )

            else
                findNodeFromExclusive iterator pred path node

        Nothing ->
            Nothing


map : (NodePath -> EditorNode -> EditorNode) -> EditorNode -> EditorNode
map =
    mapRec []


mapRec : NodePath -> (NodePath -> EditorNode -> EditorNode) -> EditorNode -> EditorNode
mapRec path func node =
    let
        applied =
            func path node
    in
    case applied of
        BlockNodeWrapper blockNode ->
            BlockNodeWrapper
                { blockNode
                    | childNodes =
                        case blockNode.childNodes of
                            BlockArray a ->
                                BlockArray <|
                                    Array.indexedMap
                                        (\i v ->
                                            case mapRec (path ++ [ i ]) func (BlockNodeWrapper v) of
                                                BlockNodeWrapper b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        a

                            InlineLeafArray a ->
                                InlineLeafArray <|
                                    Array.indexedMap
                                        (\i v ->
                                            case mapRec (path ++ [ i ]) func (InlineLeafWrapper v) of
                                                InlineLeafWrapper b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        a

                            Leaf ->
                                Leaf
                }

        InlineLeafWrapper inlineLeaf ->
            InlineLeafWrapper inlineLeaf


foldl : (NodePath -> EditorNode -> b -> b) -> b -> EditorNode -> b
foldl =
    foldlRec []


foldlRec : NodePath -> (NodePath -> EditorNode -> b -> b) -> b -> EditorNode -> b
foldlRec path func acc node =
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
                    foldlRec (path ++ [ index ]) func agg childNode
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


{-| Finds the closest node ancestor with inline content.
-}
findTextBlockNodeAncestor : NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findTextBlockNodeAncestor =
    findAncestorFromPath
        (\n ->
            case n.childNodes of
                InlineLeafArray _ ->
                    True

                _ ->
                    False
        )


{-| Find ancestor from path finds the closest ancestor from the given NodePath that matches the
predicate.
-}
findAncestorFromPath : (EditorBlockNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findAncestorFromPath pred path node =
    case path of
        [] ->
            Nothing

        x :: xs ->
            case node.childNodes of
                BlockArray a ->
                    case Array.get x a of
                        Nothing ->
                            Nothing

                        Just childNode ->
                            case findAncestorFromPath pred xs childNode of
                                Nothing ->
                                    if pred node then
                                        Just ( [], node )

                                    else
                                        Nothing

                                Just ( p, result ) ->
                                    Just ( x :: p, result )

                _ ->
                    if pred node then
                        Just ( [], node )

                    else
                        Nothing


{-| nodeAt returns the node at the specified NodePath if it exists.
-}
nodeAt : NodePath -> EditorBlockNode -> Maybe EditorNode
nodeAt path node =
    case path of
        [] ->
            Just <| BlockNodeWrapper node

        x :: xs ->
            case node.childNodes of
                BlockArray list ->
                    case Array.get x list of
                        Nothing ->
                            Nothing

                        Just childNode ->
                            nodeAt xs childNode

                InlineLeafArray list ->
                    case Array.get x list of
                        Nothing ->
                            Nothing

                        Just childLeafNode ->
                            if List.isEmpty xs then
                                Just <| InlineLeafWrapper childLeafNode

                            else
                                Nothing

                Leaf ->
                    Nothing



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


removeNodeAndEmptyParents : NodePath -> EditorBlockNode -> EditorBlockNode
removeNodeAndEmptyParents path node =
    case path of
        [] ->
            node

        [ x ] ->
            case node.childNodes of
                BlockArray a ->
                    { node | childNodes = BlockArray <| Array.Extra.removeAt x a }

                InlineLeafArray a ->
                    { node | childNodes = InlineLeafArray <| Array.Extra.removeAt x a }

                Leaf ->
                    node

        x :: xs ->
            case node.childNodes of
                BlockArray a ->
                    case Array.get x a of
                        Nothing ->
                            node

                        Just n ->
                            let
                                newNode =
                                    removeNodeAndEmptyParents xs n
                            in
                            case newNode.childNodes of
                                BlockArray newNodeChildren ->
                                    if Array.isEmpty newNodeChildren then
                                        { node | childNodes = BlockArray <| Array.Extra.removeAt x a }

                                    else
                                        { node | childNodes = BlockArray <| Array.set x newNode a }

                                _ ->
                                    newNode

                InlineLeafArray a ->
                    node

                Leaf ->
                    node
