module RichTextEditor.Node exposing
    ( Iterator
    , allRange
    , anyRange
    , concatMap
    , findAncestor
    , findBackwardFrom
    , findBackwardFromExclusive
    , findClosestBlockPath
    , findForwardFrom
    , findForwardFromExclusive
    , findLastPath
    , findTextBlockNodeAncestor
    , foldl
    , foldr
    , indexedFoldl
    , indexedFoldr
    , indexedMap
    , insert
    , isSelectable
    , joinBlocks
    , map
    , next
    , nodeAt
    , previous
    , removeInRange
    , removeNodeAndEmptyParents
    , replace
    , replaceWithFragment
    , splitBlockAtPathAndOffset
    , splitTextLeaf
    )

import Array exposing (Array)
import Array.Extra
import RichTextEditor.Model
    exposing
        ( Annotation
        , ChildNodes(..)
        , EditorBlockNode
        , EditorFragment(..)
        , EditorInlineLeaf(..)
        , EditorNode(..)
        , HtmlNode(..)
        , NodePath
        , TextLeafContents
        , inlineLeafArray
        , selectableAnnotation
        )
import Set


parent : NodePath -> NodePath
parent path =
    List.take (List.length path - 1) path


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
                    Array.length a.array - 1
            in
            case Array.get lastIndex a.array of
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
                            Just ( [], BlockNodeWrapper node )

                        Just b ->
                            let
                                ( p, n ) =
                                    findLastPath b
                            in
                            Just ( prevIndex :: p, n )

                InlineLeafArray a ->
                    case Array.get prevIndex a.array of
                        Nothing ->
                            Just ( [], BlockNodeWrapper node )

                        Just l ->
                            Just ( [ prevIndex ], InlineLeafWrapper l )

                Leaf ->
                    Just ( [], BlockNodeWrapper node )

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
                    case Array.get (x - 1) a.array of
                        Nothing ->
                            Just ( [], BlockNodeWrapper node )

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
                    case Array.get 0 a.array of
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
                    case Array.get (x + 1) a.array of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ x + 1 ], InlineLeafWrapper b )

                Leaf ->
                    Nothing


findForwardFrom : (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findForwardFrom =
    findNodeFrom next


findForwardFromExclusive : (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findForwardFromExclusive =
    findNodeFromExclusive next


findBackwardFrom : (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findBackwardFrom =
    findNodeFrom previous


findBackwardFromExclusive : (NodePath -> EditorNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorNode )
findBackwardFromExclusive =
    findNodeFromExclusive previous


isSelectable : EditorNode -> Bool
isSelectable node =
    case node of
        BlockNodeWrapper bn ->
            Set.member selectableAnnotation bn.parameters.annotations

        InlineLeafWrapper ln ->
            case ln of
                TextLeaf _ ->
                    True

                InlineLeaf l ->
                    Set.member selectableAnnotation l.parameters.annotations


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


concatMap : (EditorNode -> List EditorNode) -> EditorBlockNode -> EditorBlockNode
concatMap func node =
    let
        newChildren =
            case node.childNodes of
                Leaf ->
                    Leaf

                BlockArray a ->
                    let
                        c =
                            List.concatMap
                                (\x ->
                                    case x of
                                        BlockNodeWrapper v ->
                                            [ v ]

                                        InlineLeafWrapper _ ->
                                            []
                                )
                            <|
                                List.concatMap func (List.map BlockNodeWrapper (Array.toList a))
                    in
                    BlockArray <| Array.fromList (List.map (concatMap func) c)

                InlineLeafArray a ->
                    inlineLeafArray <|
                        Array.fromList
                            (List.concatMap
                                (\x ->
                                    case x of
                                        BlockNodeWrapper _ ->
                                            []

                                        InlineLeafWrapper v ->
                                            [ v ]
                                )
                             <|
                                List.concatMap func (List.map InlineLeafWrapper (Array.toList a.array))
                            )
    in
    { node | childNodes = newChildren }


map : (EditorNode -> EditorNode) -> EditorNode -> EditorNode
map func node =
    let
        applied =
            func node
    in
    case applied of
        BlockNodeWrapper blockNode ->
            BlockNodeWrapper
                { blockNode
                    | childNodes =
                        case blockNode.childNodes of
                            BlockArray a ->
                                BlockArray <|
                                    Array.map
                                        (\v ->
                                            case map func (BlockNodeWrapper v) of
                                                BlockNodeWrapper b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        a

                            InlineLeafArray a ->
                                inlineLeafArray <|
                                    Array.map
                                        (\v ->
                                            case map func (InlineLeafWrapper v) of
                                                InlineLeafWrapper b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        a.array

                            Leaf ->
                                Leaf
                }

        InlineLeafWrapper inlineLeaf ->
            InlineLeafWrapper inlineLeaf


indexedMap : (NodePath -> EditorNode -> EditorNode) -> EditorNode -> EditorNode
indexedMap =
    indexedMapRec []


indexedMapRec : NodePath -> (NodePath -> EditorNode -> EditorNode) -> EditorNode -> EditorNode
indexedMapRec path func node =
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
                                            case indexedMapRec (path ++ [ i ]) func (BlockNodeWrapper v) of
                                                BlockNodeWrapper b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        a

                            InlineLeafArray a ->
                                inlineLeafArray <|
                                    Array.indexedMap
                                        (\i v ->
                                            case indexedMapRec (path ++ [ i ]) func (InlineLeafWrapper v) of
                                                InlineLeafWrapper b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        a.array

                            Leaf ->
                                Leaf
                }

        InlineLeafWrapper inlineLeaf ->
            InlineLeafWrapper inlineLeaf


foldr : (EditorNode -> b -> b) -> b -> EditorNode -> b
foldr func acc node =
    func
        node
        (case node of
            BlockNodeWrapper blockNode ->
                let
                    children =
                        case blockNode.childNodes of
                            Leaf ->
                                Array.empty

                            InlineLeafArray a ->
                                Array.map InlineLeafWrapper a.array

                            BlockArray a ->
                                Array.map BlockNodeWrapper a
                in
                Array.foldr
                    (\childNode agg ->
                        foldr func agg childNode
                    )
                    acc
                    children

            InlineLeafWrapper _ ->
                acc
        )


foldl : (EditorNode -> b -> b) -> b -> EditorNode -> b
foldl func acc node =
    case node of
        BlockNodeWrapper blockNode ->
            let
                children =
                    case blockNode.childNodes of
                        Leaf ->
                            Array.empty

                        InlineLeafArray a ->
                            Array.map InlineLeafWrapper a.array

                        BlockArray a ->
                            Array.map BlockNodeWrapper a
            in
            Array.foldl
                (\childNode agg ->
                    foldl func agg childNode
                )
                (func node acc)
                children

        InlineLeafWrapper _ ->
            func node acc


indexedFoldr : (NodePath -> EditorNode -> b -> b) -> b -> EditorNode -> b
indexedFoldr =
    indexedFoldrRec []


indexedFoldrRec : NodePath -> (NodePath -> EditorNode -> b -> b) -> b -> EditorNode -> b
indexedFoldrRec path func acc node =
    func
        path
        node
        (case node of
            BlockNodeWrapper blockNode ->
                let
                    children =
                        Array.indexedMap Tuple.pair <|
                            case blockNode.childNodes of
                                Leaf ->
                                    Array.empty

                                InlineLeafArray a ->
                                    Array.map InlineLeafWrapper a.array

                                BlockArray a ->
                                    Array.map BlockNodeWrapper a
                in
                Array.foldr
                    (\( index, childNode ) agg ->
                        indexedFoldrRec (path ++ [ index ]) func agg childNode
                    )
                    acc
                    children

            InlineLeafWrapper _ ->
                acc
        )


indexedFoldl : (NodePath -> EditorNode -> b -> b) -> b -> EditorNode -> b
indexedFoldl =
    indexedFoldlRec []


indexedFoldlRec : NodePath -> (NodePath -> EditorNode -> b -> b) -> b -> EditorNode -> b
indexedFoldlRec path func acc node =
    case node of
        BlockNodeWrapper blockNode ->
            let
                children =
                    Array.indexedMap Tuple.pair <|
                        case blockNode.childNodes of
                            Leaf ->
                                Array.empty

                            InlineLeafArray a ->
                                Array.map InlineLeafWrapper a.array

                            BlockArray a ->
                                Array.map BlockNodeWrapper a
            in
            Array.foldl
                (\( index, childNode ) agg ->
                    indexedFoldlRec (path ++ [ index ]) func agg childNode
                )
                (func path node acc)
                children

        InlineLeafWrapper _ ->
            func path node acc



{- replaceNodeWithFragment replaces the node at the node path with the given fragment -}


replaceWithFragment : NodePath -> EditorFragment -> EditorBlockNode -> Result String EditorBlockNode
replaceWithFragment path fragment root =
    case path of
        [] ->
            Err "Invalid path"

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
                                        inlineLeafArray
                                            (Array.append
                                                (Array.append
                                                    (Array.Extra.sliceUntil x a.array)
                                                    leaves
                                                )
                                                (Array.Extra.sliceFrom (x + 1) a.array)
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
                            case replaceWithFragment xs fragment node of
                                Ok n ->
                                    Ok { root | childNodes = BlockArray (Array.set x n a) }

                                Err v ->
                                    Err v

                InlineLeafArray _ ->
                    Err "I received an invalid path, I reached an inline leaf array but I still have more path left."

                Leaf ->
                    Err "I received an invalid path, I am on a leaf node, but I still have more path left."



{- replaceNode replaces the node at the nodepath with the given editor node -}


replace : NodePath -> EditorNode -> EditorBlockNode -> Result String EditorBlockNode
replace path node root =
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
            replaceWithFragment path fragment root


{-| Finds the closest node ancestor with inline content.
-}
findTextBlockNodeAncestor : NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findTextBlockNodeAncestor =
    findAncestor
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
findAncestor : (EditorBlockNode -> Bool) -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findAncestor pred path node =
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
                            case findAncestor pred xs childNode of
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
                BlockArray arr ->
                    case Array.get x arr of
                        Nothing ->
                            Nothing

                        Just childNode ->
                            nodeAt xs childNode

                InlineLeafArray a ->
                    case Array.get x a.array of
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


removeInRange : NodePath -> NodePath -> EditorBlockNode -> EditorBlockNode
removeInRange start end node =
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
                        Array.length a.array

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
                            { node | childNodes = BlockArray <| Array.set startIndex (removeInRange startRest endRest b) a }

            InlineLeafArray a ->
                if List.isEmpty startRest && List.isEmpty endRest then
                    { node | childNodes = inlineLeafArray <| Array.Extra.removeAt startIndex a.array }

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
                                    Array.fromList [ removeInRange startRest endRest b ]

                    rightRest =
                        if List.isEmpty endRest then
                            Array.empty

                        else
                            case Array.get endIndex a of
                                Nothing ->
                                    Array.empty

                                Just b ->
                                    Array.fromList [ removeInRange startRest endRest b ]
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
                            a.array

                    right =
                        Array.Extra.sliceFrom
                            (if List.isEmpty endRest then
                                endIndex + 1

                             else
                                endIndex
                            )
                            a.array
                in
                { node | childNodes = inlineLeafArray <| Array.append left right }

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
                    { node | childNodes = inlineLeafArray <| Array.Extra.removeAt x a.array }

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

                                InlineLeafArray newNodeChildren ->
                                    if Array.isEmpty newNodeChildren.array then
                                        { node | childNodes = BlockArray <| Array.Extra.removeAt x a }

                                    else
                                        { node | childNodes = BlockArray <| Array.set x newNode a }

                                _ ->
                                    { node | childNodes = BlockArray <| Array.set x newNode a }

                InlineLeafArray _ ->
                    node

                Leaf ->
                    node


splitTextLeaf : Int -> TextLeafContents -> ( TextLeafContents, TextLeafContents )
splitTextLeaf offset leaf =
    ( { leaf | text = String.left offset leaf.text }, { leaf | text = String.dropLeft offset leaf.text } )


splitBlockAtPathAndOffset : NodePath -> Int -> EditorBlockNode -> Maybe ( EditorBlockNode, EditorBlockNode )
splitBlockAtPathAndOffset path offset node =
    case path of
        [] ->
            case node.childNodes of
                BlockArray a ->
                    Just
                        ( { node | childNodes = BlockArray (Array.Extra.sliceUntil offset a) }
                        , { node | childNodes = BlockArray (Array.Extra.sliceFrom offset a) }
                        )

                InlineLeafArray a ->
                    Just
                        ( { node | childNodes = inlineLeafArray (Array.Extra.sliceUntil offset a.array) }
                        , { node | childNodes = inlineLeafArray (Array.Extra.sliceFrom offset a.array) }
                        )

                Leaf ->
                    Just ( node, node )

        x :: xs ->
            case node.childNodes of
                BlockArray a ->
                    case Array.get x a of
                        Nothing ->
                            Nothing

                        Just n ->
                            case splitBlockAtPathAndOffset xs offset n of
                                Nothing ->
                                    Nothing

                                Just ( before, after ) ->
                                    Just
                                        ( { node | childNodes = BlockArray (Array.append (Array.Extra.sliceUntil x a) (Array.fromList [ before ])) }
                                        , { node | childNodes = BlockArray (Array.append (Array.fromList [ after ]) (Array.Extra.sliceFrom (x + 1) a)) }
                                        )

                InlineLeafArray a ->
                    case Array.get x a.array of
                        Nothing ->
                            Nothing

                        Just n ->
                            case n of
                                TextLeaf tl ->
                                    let
                                        ( before, after ) =
                                            splitTextLeaf offset tl
                                    in
                                    Just
                                        ( { node | childNodes = inlineLeafArray (Array.set x (TextLeaf before) (Array.Extra.sliceUntil (x + 1) a.array)) }
                                        , { node | childNodes = inlineLeafArray (Array.set 0 (TextLeaf after) (Array.Extra.sliceFrom x a.array)) }
                                        )

                                InlineLeaf _ ->
                                    Just
                                        ( { node | childNodes = inlineLeafArray (Array.Extra.sliceUntil x a.array) }
                                        , { node | childNodes = inlineLeafArray (Array.Extra.sliceFrom x a.array) }
                                        )

                Leaf ->
                    Nothing


allRange : (EditorNode -> Bool) -> NodePath -> NodePath -> EditorBlockNode -> Bool
allRange pred start end root =
    if start > end then
        True

    else
        case nodeAt start root of
            Nothing ->
                -- In the case of an invalid path, just return true.
                True

            Just node ->
                if pred node then
                    case next start root of
                        Nothing ->
                            True

                        Just ( nextPath, _ ) ->
                            allRange pred nextPath end root

                else
                    False


anyRange : (EditorNode -> Bool) -> NodePath -> NodePath -> EditorBlockNode -> Bool
anyRange pred start end root =
    not <| allRange (\x -> not <| pred x) start end root


findClosestBlockPath : NodePath -> EditorBlockNode -> NodePath
findClosestBlockPath path node =
    case nodeAt path node of
        Nothing ->
            []

        Just n ->
            case n of
                BlockNodeWrapper _ ->
                    path

                InlineLeafWrapper _ ->
                    parent path


joinBlocks : EditorBlockNode -> EditorBlockNode -> Maybe EditorBlockNode
joinBlocks b1 b2 =
    case b1.childNodes of
        BlockArray a1 ->
            case b2.childNodes of
                BlockArray a2 ->
                    Just { b1 | childNodes = BlockArray (Array.append a1 a2) }

                _ ->
                    Nothing

        InlineLeafArray a1 ->
            case b2.childNodes of
                InlineLeafArray a2 ->
                    Just { b1 | childNodes = inlineLeafArray (Array.append a1.array a2.array) }

                _ ->
                    Nothing

        Leaf ->
            Nothing


insert : NodePath -> EditorFragment -> EditorBlockNode -> Result String EditorBlockNode
insert path fragment root =
    case nodeAt path root of
        Nothing ->
            Err "There is no node at this path, so I cannot insert after it"

        Just node ->
            case node of
                InlineLeafWrapper il ->
                    case fragment of
                        InlineLeafFragment a ->
                            let
                                newFragment =
                                    InlineLeafFragment <| Array.fromList (il :: Array.toList a)
                            in
                            replaceWithFragment path newFragment root

                        BlockNodeFragment a ->
                            Err "I cannot insert a block node fragment into an inline leaf fragment"

                BlockNodeWrapper bn ->
                    case fragment of
                        BlockNodeFragment a ->
                            let
                                newFragment =
                                    BlockNodeFragment <| Array.fromList (bn :: Array.toList a)
                            in
                            replaceWithFragment path newFragment root

                        InlineLeafFragment a ->
                            Err "I cannot insert an inline leaf fragment fragment into an block node fragment"
