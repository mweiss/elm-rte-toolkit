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
    , insertAfter
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
import RichTextEditor.Model.Annotation exposing (selectableAnnotation)
import RichTextEditor.Model.Node exposing (BlockNode, ChildNodes(..), EditorInlineLeaf(..), Fragment(..), Node(..), Path, TextLeafParameters, annotationsFromElementParameters, blockArray, childNodes, elementParametersFromBlockNode, elementParametersFromInlineLeafParameters, fromBlockArray, fromInlineArray, inlineLeafArray, text, withChildNodes, withText)
import RichTextEditor.NodePath exposing (parent)
import Set


findLastPath : BlockNode -> ( Path, Node )
findLastPath node =
    case childNodes node of
        BlockChildren a ->
            let
                arr =
                    fromBlockArray a

                lastIndex =
                    Array.length arr - 1
            in
            case Array.get lastIndex arr of
                Nothing ->
                    ( [], BlockNodeWrapper node )

                Just b ->
                    let
                        ( p, n ) =
                            findLastPath b
                    in
                    ( lastIndex :: p, n )

        InlineChildren a ->
            let
                array =
                    fromInlineArray a

                lastIndex =
                    Array.length array - 1
            in
            case Array.get lastIndex array of
                Nothing ->
                    ( [], BlockNodeWrapper node )

                Just l ->
                    ( [ lastIndex ], InlineLeafWrapper l )

        Leaf ->
            ( [], BlockNodeWrapper node )


type alias Iterator =
    Path -> BlockNode -> Maybe ( Path, Node )


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
            case childNodes node of
                BlockChildren a ->
                    case Array.get prevIndex (fromBlockArray a) of
                        Nothing ->
                            Just ( [], BlockNodeWrapper node )

                        Just b ->
                            let
                                ( p, n ) =
                                    findLastPath b
                            in
                            Just ( prevIndex :: p, n )

                InlineChildren a ->
                    case Array.get prevIndex (fromInlineArray a) of
                        Nothing ->
                            Just ( [], BlockNodeWrapper node )

                        Just l ->
                            Just ( [ prevIndex ], InlineLeafWrapper l )

                Leaf ->
                    Just ( [], BlockNodeWrapper node )

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    case Array.get x (fromBlockArray a) of
                        Nothing ->
                            Nothing

                        Just b ->
                            case previous xs b of
                                Nothing ->
                                    Just ( [ x ], BlockNodeWrapper b )

                                Just ( p, n ) ->
                                    Just ( x :: p, n )

                InlineChildren a ->
                    case Array.get (x - 1) (fromInlineArray a) of
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
            case childNodes node of
                BlockChildren a ->
                    case Array.get 0 (fromBlockArray a) of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ 0 ], BlockNodeWrapper b )

                InlineChildren a ->
                    case Array.get 0 (fromInlineArray a) of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ 0 ], InlineLeafWrapper b )

                Leaf ->
                    Nothing

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    let
                        arr =
                            fromBlockArray a
                    in
                    case Array.get x arr of
                        Nothing ->
                            Nothing

                        Just b ->
                            case next xs b of
                                Nothing ->
                                    case Array.get (x + 1) arr of
                                        Nothing ->
                                            Nothing

                                        Just bNext ->
                                            Just ( [ x + 1 ], BlockNodeWrapper bNext )

                                Just ( p, n ) ->
                                    Just ( x :: p, n )

                InlineChildren a ->
                    case Array.get (x + 1) (fromInlineArray a) of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ x + 1 ], InlineLeafWrapper b )

                Leaf ->
                    Nothing


findForwardFrom : (Path -> Node -> Bool) -> Path -> BlockNode -> Maybe ( Path, Node )
findForwardFrom =
    findNodeFrom next


findForwardFromExclusive : (Path -> Node -> Bool) -> Path -> BlockNode -> Maybe ( Path, Node )
findForwardFromExclusive =
    findNodeFromExclusive next


findBackwardFrom : (Path -> Node -> Bool) -> Path -> BlockNode -> Maybe ( Path, Node )
findBackwardFrom =
    findNodeFrom previous


findBackwardFromExclusive : (Path -> Node -> Bool) -> Path -> BlockNode -> Maybe ( Path, Node )
findBackwardFromExclusive =
    findNodeFromExclusive previous


isSelectable : Node -> Bool
isSelectable node =
    case node of
        BlockNodeWrapper bn ->
            Set.member selectableAnnotation (annotationsFromElementParameters (elementParametersFromBlockNode bn))

        InlineLeafWrapper ln ->
            case ln of
                TextLeaf _ ->
                    True

                InlineLeaf l ->
                    Set.member selectableAnnotation (annotationsFromElementParameters (elementParametersFromInlineLeafParameters l))


findNodeFromExclusive : Iterator -> (Path -> Node -> Bool) -> Path -> BlockNode -> Maybe ( Path, Node )
findNodeFromExclusive iterator pred path node =
    case iterator path node of
        Nothing ->
            Nothing

        Just ( nextPath, _ ) ->
            findNodeFrom iterator pred nextPath node


findNodeFrom : Iterator -> (Path -> Node -> Bool) -> Path -> BlockNode -> Maybe ( Path, Node )
findNodeFrom iterator pred path node =
    case nodeAt path node of
        Just n ->
            if pred path n then
                Just ( path, n )

            else
                findNodeFromExclusive iterator pred path node

        Nothing ->
            Nothing


concatMap : (Node -> List Node) -> BlockNode -> BlockNode
concatMap func node =
    let
        newChildren =
            case childNodes node of
                Leaf ->
                    Leaf

                BlockChildren a ->
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
                                List.concatMap func (List.map BlockNodeWrapper (Array.toList (fromBlockArray a)))
                    in
                    blockArray <| Array.fromList (List.map (concatMap func) c)

                InlineChildren a ->
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
                                List.concatMap func (List.map InlineLeafWrapper (Array.toList (fromInlineArray a)))
                            )
    in
    node |> withChildNodes newChildren


map : (Node -> Node) -> Node -> Node
map func node =
    let
        applied =
            func node
    in
    case applied of
        BlockNodeWrapper blockNode ->
            BlockNodeWrapper <|
                (blockNode
                    |> withChildNodes
                        (case childNodes blockNode of
                            BlockChildren a ->
                                blockArray <|
                                    Array.map
                                        (\v ->
                                            case map func (BlockNodeWrapper v) of
                                                BlockNodeWrapper b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        (fromBlockArray a)

                            InlineChildren a ->
                                inlineLeafArray <|
                                    Array.map
                                        (\v ->
                                            case map func (InlineLeafWrapper v) of
                                                InlineLeafWrapper b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        (fromInlineArray a)

                            Leaf ->
                                Leaf
                        )
                )

        InlineLeafWrapper inlineLeaf ->
            InlineLeafWrapper inlineLeaf


indexedMap : (Path -> Node -> Node) -> Node -> Node
indexedMap =
    indexedMapRec []


indexedMapRec : Path -> (Path -> Node -> Node) -> Node -> Node
indexedMapRec path func node =
    let
        applied =
            func path node
    in
    case applied of
        BlockNodeWrapper blockNode ->
            let
                cn =
                    case childNodes blockNode of
                        BlockChildren a ->
                            blockArray <|
                                Array.indexedMap
                                    (\i v ->
                                        case indexedMapRec (path ++ [ i ]) func (BlockNodeWrapper v) of
                                            BlockNodeWrapper b ->
                                                b

                                            _ ->
                                                v
                                    )
                                    (fromBlockArray a)

                        InlineChildren a ->
                            inlineLeafArray <|
                                Array.indexedMap
                                    (\i v ->
                                        case indexedMapRec (path ++ [ i ]) func (InlineLeafWrapper v) of
                                            InlineLeafWrapper b ->
                                                b

                                            _ ->
                                                v
                                    )
                                    (fromInlineArray a)

                        Leaf ->
                            Leaf
            in
            BlockNodeWrapper (blockNode |> withChildNodes cn)

        InlineLeafWrapper inlineLeaf ->
            InlineLeafWrapper inlineLeaf


foldr : (Node -> b -> b) -> b -> Node -> b
foldr func acc node =
    func
        node
        (case node of
            BlockNodeWrapper blockNode ->
                let
                    children =
                        case childNodes blockNode of
                            Leaf ->
                                Array.empty

                            InlineChildren a ->
                                Array.map InlineLeafWrapper (fromInlineArray a)

                            BlockChildren a ->
                                Array.map BlockNodeWrapper (fromBlockArray a)
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


foldl : (Node -> b -> b) -> b -> Node -> b
foldl func acc node =
    case node of
        BlockNodeWrapper blockNode ->
            let
                children =
                    case childNodes blockNode of
                        Leaf ->
                            Array.empty

                        InlineChildren a ->
                            Array.map InlineLeafWrapper (fromInlineArray a)

                        BlockChildren a ->
                            Array.map BlockNodeWrapper (fromBlockArray a)
            in
            Array.foldl
                (\childNode agg ->
                    foldl func agg childNode
                )
                (func node acc)
                children

        InlineLeafWrapper _ ->
            func node acc


indexedFoldr : (Path -> Node -> b -> b) -> b -> Node -> b
indexedFoldr =
    indexedFoldrRec []


indexedFoldrRec : Path -> (Path -> Node -> b -> b) -> b -> Node -> b
indexedFoldrRec path func acc node =
    func
        path
        node
        (case node of
            BlockNodeWrapper blockNode ->
                let
                    children =
                        Array.indexedMap Tuple.pair <|
                            case childNodes blockNode of
                                Leaf ->
                                    Array.empty

                                InlineChildren a ->
                                    Array.map InlineLeafWrapper (fromInlineArray a)

                                BlockChildren a ->
                                    Array.map BlockNodeWrapper (fromBlockArray a)
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


indexedFoldl : (Path -> Node -> b -> b) -> b -> Node -> b
indexedFoldl =
    indexedFoldlRec []


indexedFoldlRec : Path -> (Path -> Node -> b -> b) -> b -> Node -> b
indexedFoldlRec path func acc node =
    case node of
        BlockNodeWrapper blockNode ->
            let
                children =
                    Array.indexedMap Tuple.pair <|
                        case childNodes blockNode of
                            Leaf ->
                                Array.empty

                            InlineChildren a ->
                                Array.map InlineLeafWrapper (fromInlineArray a)

                            BlockChildren a ->
                                Array.map BlockNodeWrapper (fromBlockArray a)
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


replaceWithFragment : Path -> Fragment -> BlockNode -> Result String BlockNode
replaceWithFragment path fragment root =
    case path of
        [] ->
            Err "Invalid path"

        [ x ] ->
            case childNodes root of
                BlockChildren a ->
                    case fragment of
                        BlockNodeFragment blocks ->
                            let
                                arr =
                                    fromBlockArray a
                            in
                            Ok <|
                                (root
                                    |> withChildNodes
                                        (blockArray
                                            (Array.append
                                                (Array.append
                                                    (Array.Extra.sliceUntil x arr)
                                                    blocks
                                                )
                                                (Array.Extra.sliceFrom (x + 1) arr)
                                            )
                                        )
                                )

                        InlineLeafFragment _ ->
                            Err "I cannot replace a block fragment with an inline leaf fragment"

                InlineChildren a ->
                    case fragment of
                        InlineLeafFragment leaves ->
                            let
                                arr =
                                    fromInlineArray a
                            in
                            Ok <|
                                (root
                                    |> withChildNodes
                                        (inlineLeafArray
                                            (Array.append
                                                (Array.append
                                                    (Array.Extra.sliceUntil x arr)
                                                    leaves
                                                )
                                                (Array.Extra.sliceFrom (x + 1) arr)
                                            )
                                        )
                                )

                        BlockNodeFragment _ ->
                            Err "I cannot replace an inline fragment with an block fragment"

                Leaf ->
                    Err "Not implemented"

        x :: xs ->
            case childNodes root of
                BlockChildren a ->
                    let
                        arr =
                            fromBlockArray a
                    in
                    case Array.get x arr of
                        Nothing ->
                            Err "I received an invalid path, I can't find a block node at the given index."

                        Just node ->
                            case replaceWithFragment xs fragment node of
                                Ok n ->
                                    Ok <| (root |> withChildNodes (blockArray (Array.set x n arr)))

                                Err v ->
                                    Err v

                InlineChildren _ ->
                    Err "I received an invalid path, I reached an inline leaf array but I still have more path left."

                Leaf ->
                    Err "I received an invalid path, I am on a leaf node, but I still have more path left."



{- replaceNode replaces the node at the nodepath with the given editor node -}


replace : Path -> Node -> BlockNode -> Result String BlockNode
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
findTextBlockNodeAncestor : Path -> BlockNode -> Maybe ( Path, BlockNode )
findTextBlockNodeAncestor =
    findAncestor
        (\n ->
            case childNodes n of
                InlineChildren _ ->
                    True

                _ ->
                    False
        )


{-| Find ancestor from path finds the closest ancestor from the given NodePath that matches the
predicate.
-}
findAncestor : (BlockNode -> Bool) -> Path -> BlockNode -> Maybe ( Path, BlockNode )
findAncestor pred path node =
    case path of
        [] ->
            Nothing

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    case Array.get x (fromBlockArray a) of
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
nodeAt : Path -> BlockNode -> Maybe Node
nodeAt path node =
    case path of
        [] ->
            Just <| BlockNodeWrapper node

        x :: xs ->
            case childNodes node of
                BlockChildren arr ->
                    case Array.get x (fromBlockArray arr) of
                        Nothing ->
                            Nothing

                        Just childNode ->
                            nodeAt xs childNode

                InlineChildren a ->
                    case Array.get x (fromInlineArray a) of
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


removeInRange : Path -> Path -> BlockNode -> BlockNode
removeInRange start end node =
    let
        startIndex =
            Maybe.withDefault 0 (List.head start)

        startRest =
            Maybe.withDefault [] (List.tail start)

        endIndex =
            Maybe.withDefault
                (case childNodes node of
                    BlockChildren a ->
                        Array.length (fromBlockArray a)

                    InlineChildren a ->
                        Array.length (fromInlineArray a)

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
        case childNodes node of
            BlockChildren a ->
                let
                    array =
                        fromBlockArray a
                in
                if List.isEmpty startRest && List.isEmpty endRest then
                    node |> withChildNodes (blockArray <| Array.Extra.removeAt startIndex array)

                else
                    case Array.get startIndex array of
                        Nothing ->
                            node

                        Just b ->
                            node |> withChildNodes (blockArray <| Array.set startIndex (removeInRange startRest endRest b) array)

            InlineChildren a ->
                if List.isEmpty startRest && List.isEmpty endRest then
                    node |> withChildNodes (inlineLeafArray <| Array.Extra.removeAt startIndex (fromInlineArray a))

                else
                    node

            Leaf ->
                node

    else
        case childNodes node of
            BlockChildren a ->
                let
                    arr =
                        fromBlockArray a

                    left =
                        Array.Extra.sliceUntil startIndex arr

                    right =
                        Array.Extra.sliceFrom (endIndex + 1) arr

                    leftRest =
                        if List.isEmpty startRest then
                            Array.empty

                        else
                            case Array.get startIndex arr of
                                Nothing ->
                                    Array.empty

                                Just b ->
                                    Array.fromList [ removeInRange startRest endRest b ]

                    rightRest =
                        if List.isEmpty endRest then
                            Array.empty

                        else
                            case Array.get endIndex arr of
                                Nothing ->
                                    Array.empty

                                Just b ->
                                    Array.fromList [ removeInRange startRest endRest b ]
                in
                node |> withChildNodes (blockArray <| List.foldr Array.append Array.empty [ left, leftRest, rightRest, right ])

            InlineChildren a ->
                let
                    arr =
                        fromInlineArray a

                    left =
                        Array.Extra.sliceUntil
                            (if List.isEmpty startRest then
                                startIndex

                             else
                                startIndex + 1
                            )
                            arr

                    right =
                        Array.Extra.sliceFrom
                            (if List.isEmpty endRest then
                                endIndex + 1

                             else
                                endIndex
                            )
                            arr
                in
                node |> withChildNodes (inlineLeafArray <| Array.append left right)

            Leaf ->
                node


removeNodeAndEmptyParents : Path -> BlockNode -> BlockNode
removeNodeAndEmptyParents path node =
    case path of
        [] ->
            node

        [ x ] ->
            case childNodes node of
                BlockChildren a ->
                    node |> withChildNodes (blockArray <| Array.Extra.removeAt x (fromBlockArray a))

                InlineChildren a ->
                    node |> withChildNodes (inlineLeafArray <| Array.Extra.removeAt x (fromInlineArray a))

                Leaf ->
                    node

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    let
                        arr =
                            fromBlockArray a
                    in
                    case Array.get x arr of
                        Nothing ->
                            node

                        Just n ->
                            let
                                newNode =
                                    removeNodeAndEmptyParents xs n
                            in
                            case childNodes newNode of
                                BlockChildren newNodeChildren ->
                                    let
                                        newChildNodes =
                                            fromBlockArray newNodeChildren
                                    in
                                    if Array.isEmpty newChildNodes then
                                        node |> withChildNodes (blockArray <| Array.Extra.removeAt x arr)

                                    else
                                        node |> withChildNodes (blockArray <| Array.set x newNode arr)

                                InlineChildren newNodeChildren ->
                                    let
                                        newChildNodes =
                                            fromInlineArray newNodeChildren
                                    in
                                    if Array.isEmpty newChildNodes then
                                        node |> withChildNodes (blockArray <| Array.Extra.removeAt x arr)

                                    else
                                        node |> withChildNodes (blockArray <| Array.set x newNode arr)

                                _ ->
                                    node |> withChildNodes (blockArray <| Array.set x newNode arr)

                InlineChildren _ ->
                    node

                Leaf ->
                    node


splitTextLeaf : Int -> TextLeafParameters -> ( TextLeafParameters, TextLeafParameters )
splitTextLeaf offset leaf =
    let
        leafText =
            text leaf
    in
    ( leaf |> withText (String.left offset leafText), leaf |> withText (String.dropLeft offset leafText) )


splitBlockAtPathAndOffset : Path -> Int -> BlockNode -> Maybe ( BlockNode, BlockNode )
splitBlockAtPathAndOffset path offset node =
    case path of
        [] ->
            case childNodes node of
                BlockChildren a ->
                    let
                        arr =
                            fromBlockArray a
                    in
                    Just
                        ( node |> withChildNodes (blockArray (Array.Extra.sliceUntil offset arr))
                        , node |> withChildNodes (blockArray (Array.Extra.sliceFrom offset arr))
                        )

                InlineChildren a ->
                    let
                        arr =
                            fromInlineArray a
                    in
                    Just
                        ( node |> withChildNodes (inlineLeafArray (Array.Extra.sliceUntil offset arr))
                        , node |> withChildNodes (inlineLeafArray (Array.Extra.sliceFrom offset arr))
                        )

                Leaf ->
                    Just ( node, node )

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    let
                        arr =
                            fromBlockArray a
                    in
                    case Array.get x arr of
                        Nothing ->
                            Nothing

                        Just n ->
                            case splitBlockAtPathAndOffset xs offset n of
                                Nothing ->
                                    Nothing

                                Just ( before, after ) ->
                                    Just
                                        ( node |> withChildNodes (blockArray (Array.append (Array.Extra.sliceUntil x arr) (Array.fromList [ before ])))
                                        , node |> withChildNodes (blockArray (Array.append (Array.fromList [ after ]) (Array.Extra.sliceFrom (x + 1) arr)))
                                        )

                InlineChildren a ->
                    let
                        arr =
                            fromInlineArray a
                    in
                    case Array.get x arr of
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
                                        ( node |> withChildNodes (inlineLeafArray (Array.set x (TextLeaf before) (Array.Extra.sliceUntil (x + 1) arr)))
                                        , node |> withChildNodes (inlineLeafArray (Array.set 0 (TextLeaf after) (Array.Extra.sliceFrom x arr)))
                                        )

                                InlineLeaf _ ->
                                    Just
                                        ( node |> withChildNodes (inlineLeafArray (Array.Extra.sliceUntil x arr))
                                        , node |> withChildNodes (inlineLeafArray (Array.Extra.sliceFrom x arr))
                                        )

                Leaf ->
                    Nothing


allRange : (Node -> Bool) -> Path -> Path -> BlockNode -> Bool
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


anyRange : (Node -> Bool) -> Path -> Path -> BlockNode -> Bool
anyRange pred start end root =
    not <| allRange (\x -> not <| pred x) start end root


findClosestBlockPath : Path -> BlockNode -> Path
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


joinBlocks : BlockNode -> BlockNode -> Maybe BlockNode
joinBlocks b1 b2 =
    case childNodes b1 of
        BlockChildren a1 ->
            case childNodes b2 of
                BlockChildren a2 ->
                    Just <| (b1 |> withChildNodes (blockArray (Array.append (fromBlockArray a1) (fromBlockArray a2))))

                _ ->
                    Nothing

        InlineChildren a1 ->
            case childNodes b2 of
                InlineChildren a2 ->
                    Just <| (b1 |> withChildNodes (inlineLeafArray (Array.append (fromInlineArray a1) (fromInlineArray a2))))

                _ ->
                    Nothing

        Leaf ->
            Nothing


insertAfter : Path -> Fragment -> BlockNode -> Result String BlockNode
insertAfter path fragment root =
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

                        BlockNodeFragment _ ->
                            Err "I cannot insert a block node fragment into an inline leaf fragment"

                BlockNodeWrapper bn ->
                    case fragment of
                        BlockNodeFragment a ->
                            let
                                newFragment =
                                    BlockNodeFragment <| Array.fromList (bn :: Array.toList a)
                            in
                            replaceWithFragment path newFragment root

                        InlineLeafFragment _ ->
                            Err "I cannot insert an inline leaf fragment fragment into an block node fragment"
