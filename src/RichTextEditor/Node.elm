module RichTextEditor.Node exposing
    ( Fragment(..)
    , Iterator
    , Node(..)
    , allRange
    , anyRange
    , concatMap
    , findAncestor
    , findBackwardFrom
    , findBackwardFromExclusive
    , findClosestBlockPath
    , findForwardFrom
    , findForwardFromExclusive
    , findTextBlockNodeAncestor
    , foldl
    , foldlRange
    , foldr
    , foldrRange
    , indexedFoldl
    , indexedFoldr
    , indexedMap
    , insertAfter
    , insertBefore
    , isSelectable
    , joinBlocks
    , last
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
import RichTextEditor.Internal.Constants exposing (selectable)
import RichTextEditor.Model.Element as Element
import RichTextEditor.Model.InlineElement as InlineElement
import RichTextEditor.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , Path
        , blockChildren
        , childNodes
        , element
        , inlineChildren
        , parent
        , toBlockArray
        , toInlineArray
        , withChildNodes
        )
import RichTextEditor.Model.Text exposing (Text, text, withText)
import Set


type Node
    = Block Block
    | Inline Inline


type Fragment
    = BlockNodeFragment (Array Block)
    | InlineLeafFragment (Array Inline)


last : Block -> ( Path, Node )
last node =
    case childNodes node of
        BlockChildren a ->
            let
                arr =
                    toBlockArray a

                lastIndex =
                    Array.length arr - 1
            in
            case Array.get lastIndex arr of
                Nothing ->
                    ( [], Block node )

                Just b ->
                    let
                        ( p, n ) =
                            last b
                    in
                    ( lastIndex :: p, n )

        InlineChildren a ->
            let
                array =
                    toInlineArray a

                lastIndex =
                    Array.length array - 1
            in
            case Array.get lastIndex array of
                Nothing ->
                    ( [], Block node )

                Just l ->
                    ( [ lastIndex ], Inline l )

        Leaf ->
            ( [], Block node )


type alias Iterator =
    Path -> Block -> Maybe ( Path, Node )


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
                    case Array.get prevIndex (toBlockArray a) of
                        Nothing ->
                            Just ( [], Block node )

                        Just b ->
                            let
                                ( p, n ) =
                                    last b
                            in
                            Just ( prevIndex :: p, n )

                InlineChildren a ->
                    case Array.get prevIndex (toInlineArray a) of
                        Nothing ->
                            Just ( [], Block node )

                        Just l ->
                            Just ( [ prevIndex ], Inline l )

                Leaf ->
                    Just ( [], Block node )

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    case Array.get x (toBlockArray a) of
                        Nothing ->
                            Nothing

                        Just b ->
                            case previous xs b of
                                Nothing ->
                                    Just ( [ x ], Block b )

                                Just ( p, n ) ->
                                    Just ( x :: p, n )

                InlineChildren a ->
                    case Array.get (x - 1) (toInlineArray a) of
                        Nothing ->
                            Just ( [], Block node )

                        Just l ->
                            Just ( [ x - 1 ], Inline l )

                Leaf ->
                    Nothing


next : Iterator
next path node =
    case path of
        [] ->
            case childNodes node of
                BlockChildren a ->
                    case Array.get 0 (toBlockArray a) of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ 0 ], Block b )

                InlineChildren a ->
                    case Array.get 0 (toInlineArray a) of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ 0 ], Inline b )

                Leaf ->
                    Nothing

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    let
                        arr =
                            toBlockArray a
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
                                            Just ( [ x + 1 ], Block bNext )

                                Just ( p, n ) ->
                                    Just ( x :: p, n )

                InlineChildren a ->
                    case Array.get (x + 1) (toInlineArray a) of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( [ x + 1 ], Inline b )

                Leaf ->
                    Nothing


findForwardFrom : (Path -> Node -> Bool) -> Path -> Block -> Maybe ( Path, Node )
findForwardFrom =
    findNodeFrom next


findForwardFromExclusive : (Path -> Node -> Bool) -> Path -> Block -> Maybe ( Path, Node )
findForwardFromExclusive =
    findNodeFromExclusive next


findBackwardFrom : (Path -> Node -> Bool) -> Path -> Block -> Maybe ( Path, Node )
findBackwardFrom =
    findNodeFrom previous


findBackwardFromExclusive : (Path -> Node -> Bool) -> Path -> Block -> Maybe ( Path, Node )
findBackwardFromExclusive =
    findNodeFromExclusive previous


isSelectable : Node -> Bool
isSelectable node =
    case node of
        Block bn ->
            Set.member selectable (Element.annotations (element bn))

        Inline ln ->
            case ln of
                Text _ ->
                    True

                InlineElement l ->
                    Set.member selectable (Element.annotations (InlineElement.element l))


findNodeFromExclusive : Iterator -> (Path -> Node -> Bool) -> Path -> Block -> Maybe ( Path, Node )
findNodeFromExclusive iterator pred path node =
    case iterator path node of
        Nothing ->
            Nothing

        Just ( nextPath, _ ) ->
            findNodeFrom iterator pred nextPath node


findNodeFrom : Iterator -> (Path -> Node -> Bool) -> Path -> Block -> Maybe ( Path, Node )
findNodeFrom iterator pred path node =
    case nodeAt path node of
        Just n ->
            if pred path n then
                Just ( path, n )

            else
                findNodeFromExclusive iterator pred path node

        Nothing ->
            Nothing


concatMap : (Node -> List Node) -> Block -> Block
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
                                        Block v ->
                                            [ v ]

                                        Inline _ ->
                                            []
                                )
                            <|
                                List.concatMap func (List.map Block (Array.toList (toBlockArray a)))
                    in
                    blockChildren <| Array.fromList (List.map (concatMap func) c)

                InlineChildren a ->
                    inlineChildren <|
                        Array.fromList
                            (List.concatMap
                                (\x ->
                                    case x of
                                        Block _ ->
                                            []

                                        Inline v ->
                                            [ v ]
                                )
                             <|
                                List.concatMap func (List.map Inline (Array.toList (toInlineArray a)))
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
        Block blockNode ->
            Block <|
                (blockNode
                    |> withChildNodes
                        (case childNodes blockNode of
                            BlockChildren a ->
                                blockChildren <|
                                    Array.map
                                        (\v ->
                                            case map func (Block v) of
                                                Block b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        (toBlockArray a)

                            InlineChildren a ->
                                inlineChildren <|
                                    Array.map
                                        (\v ->
                                            case map func (Inline v) of
                                                Inline b ->
                                                    b

                                                _ ->
                                                    v
                                        )
                                        (toInlineArray a)

                            Leaf ->
                                Leaf
                        )
                )

        Inline inlineLeaf ->
            Inline inlineLeaf


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
        Block blockNode ->
            let
                cn =
                    case childNodes blockNode of
                        BlockChildren a ->
                            blockChildren <|
                                Array.indexedMap
                                    (\i v ->
                                        case indexedMapRec (path ++ [ i ]) func (Block v) of
                                            Block b ->
                                                b

                                            _ ->
                                                v
                                    )
                                    (toBlockArray a)

                        InlineChildren a ->
                            inlineChildren <|
                                Array.indexedMap
                                    (\i v ->
                                        case indexedMapRec (path ++ [ i ]) func (Inline v) of
                                            Inline b ->
                                                b

                                            _ ->
                                                v
                                    )
                                    (toInlineArray a)

                        Leaf ->
                            Leaf
            in
            Block (blockNode |> withChildNodes cn)

        Inline inlineLeaf ->
            Inline inlineLeaf


foldr : (Node -> b -> b) -> b -> Node -> b
foldr func acc node =
    func
        node
        (case node of
            Block blockNode ->
                let
                    children =
                        case childNodes blockNode of
                            Leaf ->
                                Array.empty

                            InlineChildren a ->
                                Array.map Inline (toInlineArray a)

                            BlockChildren a ->
                                Array.map Block (toBlockArray a)
                in
                Array.foldr
                    (\childNode agg ->
                        foldr func agg childNode
                    )
                    acc
                    children

            Inline _ ->
                acc
        )


foldl : (Node -> b -> b) -> b -> Node -> b
foldl func acc node =
    case node of
        Block blockNode ->
            let
                children =
                    case childNodes blockNode of
                        Leaf ->
                            Array.empty

                        InlineChildren a ->
                            Array.map Inline (toInlineArray a)

                        BlockChildren a ->
                            Array.map Block (toBlockArray a)
            in
            Array.foldl
                (\childNode agg ->
                    foldl func agg childNode
                )
                (func node acc)
                children

        Inline _ ->
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
            Block blockNode ->
                let
                    children =
                        Array.indexedMap Tuple.pair <|
                            case childNodes blockNode of
                                Leaf ->
                                    Array.empty

                                InlineChildren a ->
                                    Array.map Inline (toInlineArray a)

                                BlockChildren a ->
                                    Array.map Block (toBlockArray a)
                in
                Array.foldr
                    (\( index, childNode ) agg ->
                        indexedFoldrRec (path ++ [ index ]) func agg childNode
                    )
                    acc
                    children

            Inline _ ->
                acc
        )


indexedFoldl : (Path -> Node -> b -> b) -> b -> Node -> b
indexedFoldl =
    indexedFoldlRec []


indexedFoldlRec : Path -> (Path -> Node -> b -> b) -> b -> Node -> b
indexedFoldlRec path func acc node =
    case node of
        Block blockNode ->
            let
                children =
                    Array.indexedMap Tuple.pair <|
                        case childNodes blockNode of
                            Leaf ->
                                Array.empty

                            InlineChildren a ->
                                Array.map Inline (toInlineArray a)

                            BlockChildren a ->
                                Array.map Block (toBlockArray a)
            in
            Array.foldl
                (\( index, childNode ) agg ->
                    indexedFoldlRec (path ++ [ index ]) func agg childNode
                )
                (func path node acc)
                children

        Inline _ ->
            func path node acc


foldlRange : Path -> Path -> (Node -> b -> b) -> b -> Block -> b
foldlRange start end func acc root =
    case nodeAt start root of
        Nothing ->
            acc

        Just node ->
            foldlRangeRec start end func acc root node


foldlRangeRec : Path -> Path -> (Node -> b -> b) -> b -> Block -> Node -> b
foldlRangeRec start end func acc root node =
    if start > end then
        acc

    else
        let
            result =
                func node acc
        in
        case next start root of
            Nothing ->
                result

            Just ( p, n ) ->
                foldlRangeRec p end func result root n


foldrRange : Path -> Path -> (Node -> b -> b) -> b -> Block -> b
foldrRange start end func acc root =
    case nodeAt end root of
        Nothing ->
            acc

        Just node ->
            foldrRangeRec start end func acc root node


foldrRangeRec : Path -> Path -> (Node -> b -> b) -> b -> Block -> Node -> b
foldrRangeRec start end func acc root node =
    if start > end then
        acc

    else
        let
            result =
                func node acc
        in
        case previous end root of
            Nothing ->
                result

            Just ( p, n ) ->
                foldrRangeRec start p func result root n



{- replaceNodeWithFragment replaces the node at the node path with the given fragment -}


replaceWithFragment : Path -> Fragment -> Block -> Result String Block
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
                                    toBlockArray a
                            in
                            Ok <|
                                (root
                                    |> withChildNodes
                                        (blockChildren
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
                                    toInlineArray a
                            in
                            Ok <|
                                (root
                                    |> withChildNodes
                                        (inlineChildren
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
                            toBlockArray a
                    in
                    case Array.get x arr of
                        Nothing ->
                            Err "I received an invalid path, I can't find a block node at the given index."

                        Just node ->
                            case replaceWithFragment xs fragment node of
                                Ok n ->
                                    Ok <| (root |> withChildNodes (blockChildren (Array.set x n arr)))

                                Err v ->
                                    Err v

                InlineChildren _ ->
                    Err "I received an invalid path, I reached an inline leaf array but I still have more path left."

                Leaf ->
                    Err "I received an invalid path, I am on a leaf node, but I still have more path left."



{- replaceNode replaces the node at the nodepath with the given editor node -}


replace : Path -> Node -> Block -> Result String Block
replace path node root =
    case path of
        [] ->
            case node of
                Block n ->
                    Ok n

                Inline _ ->
                    Err "I cannot replace a block node with an inline leaf."

        _ ->
            let
                fragment =
                    case node of
                        Block n ->
                            BlockNodeFragment <| Array.fromList [ n ]

                        Inline n ->
                            InlineLeafFragment <| Array.fromList [ n ]
            in
            replaceWithFragment path fragment root


{-| Finds the closest node ancestor with inline content.
-}
findTextBlockNodeAncestor : Path -> Block -> Maybe ( Path, Block )
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
findAncestor : (Block -> Bool) -> Path -> Block -> Maybe ( Path, Block )
findAncestor pred path node =
    case path of
        [] ->
            Nothing

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    case Array.get x (toBlockArray a) of
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
nodeAt : Path -> Block -> Maybe Node
nodeAt path node =
    case path of
        [] ->
            Just <| Block node

        x :: xs ->
            case childNodes node of
                BlockChildren arr ->
                    case Array.get x (toBlockArray arr) of
                        Nothing ->
                            Nothing

                        Just childNode ->
                            nodeAt xs childNode

                InlineChildren a ->
                    case Array.get x (toInlineArray a) of
                        Nothing ->
                            Nothing

                        Just childLeafNode ->
                            if List.isEmpty xs then
                                Just <| Inline childLeafNode

                            else
                                Nothing

                Leaf ->
                    Nothing



{- This method removes all the nodes inclusive to both the start and end node path.  Note that
   an ancestor is not removed if the start path or end path is a child node.
-}


removeInRange : Path -> Path -> Block -> Block
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
                        Array.length (toBlockArray a)

                    InlineChildren a ->
                        Array.length (toInlineArray a)

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
                        toBlockArray a
                in
                if List.isEmpty startRest && List.isEmpty endRest then
                    node |> withChildNodes (blockChildren <| Array.Extra.removeAt startIndex array)

                else
                    case Array.get startIndex array of
                        Nothing ->
                            node

                        Just b ->
                            node |> withChildNodes (blockChildren <| Array.set startIndex (removeInRange startRest endRest b) array)

            InlineChildren a ->
                if List.isEmpty startRest && List.isEmpty endRest then
                    node |> withChildNodes (inlineChildren <| Array.Extra.removeAt startIndex (toInlineArray a))

                else
                    node

            Leaf ->
                node

    else
        case childNodes node of
            BlockChildren a ->
                let
                    arr =
                        toBlockArray a

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
                node |> withChildNodes (blockChildren <| List.foldr Array.append Array.empty [ left, leftRest, rightRest, right ])

            InlineChildren a ->
                let
                    arr =
                        toInlineArray a

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
                node |> withChildNodes (inlineChildren <| Array.append left right)

            Leaf ->
                node


removeNodeAndEmptyParents : Path -> Block -> Block
removeNodeAndEmptyParents path node =
    case path of
        [] ->
            node

        [ x ] ->
            case childNodes node of
                BlockChildren a ->
                    node |> withChildNodes (blockChildren <| Array.Extra.removeAt x (toBlockArray a))

                InlineChildren a ->
                    node |> withChildNodes (inlineChildren <| Array.Extra.removeAt x (toInlineArray a))

                Leaf ->
                    node

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    let
                        arr =
                            toBlockArray a
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
                                            toBlockArray newNodeChildren
                                    in
                                    if Array.isEmpty newChildNodes then
                                        node |> withChildNodes (blockChildren <| Array.Extra.removeAt x arr)

                                    else
                                        node |> withChildNodes (blockChildren <| Array.set x newNode arr)

                                InlineChildren newNodeChildren ->
                                    let
                                        newChildNodes =
                                            toInlineArray newNodeChildren
                                    in
                                    if Array.isEmpty newChildNodes then
                                        node |> withChildNodes (blockChildren <| Array.Extra.removeAt x arr)

                                    else
                                        node |> withChildNodes (blockChildren <| Array.set x newNode arr)

                                _ ->
                                    node |> withChildNodes (blockChildren <| Array.set x newNode arr)

                InlineChildren _ ->
                    node

                Leaf ->
                    node


splitTextLeaf : Int -> Text -> ( Text, Text )
splitTextLeaf offset leaf =
    let
        leafText =
            text leaf
    in
    ( leaf |> withText (String.left offset leafText), leaf |> withText (String.dropLeft offset leafText) )


splitBlockAtPathAndOffset : Path -> Int -> Block -> Maybe ( Block, Block )
splitBlockAtPathAndOffset path offset node =
    case path of
        [] ->
            case childNodes node of
                BlockChildren a ->
                    let
                        arr =
                            toBlockArray a
                    in
                    Just
                        ( node |> withChildNodes (blockChildren (Array.Extra.sliceUntil offset arr))
                        , node |> withChildNodes (blockChildren (Array.Extra.sliceFrom offset arr))
                        )

                InlineChildren a ->
                    let
                        arr =
                            toInlineArray a
                    in
                    Just
                        ( node |> withChildNodes (inlineChildren (Array.Extra.sliceUntil offset arr))
                        , node |> withChildNodes (inlineChildren (Array.Extra.sliceFrom offset arr))
                        )

                Leaf ->
                    Just ( node, node )

        x :: xs ->
            case childNodes node of
                BlockChildren a ->
                    let
                        arr =
                            toBlockArray a
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
                                        ( node |> withChildNodes (blockChildren (Array.append (Array.Extra.sliceUntil x arr) (Array.fromList [ before ])))
                                        , node |> withChildNodes (blockChildren (Array.append (Array.fromList [ after ]) (Array.Extra.sliceFrom (x + 1) arr)))
                                        )

                InlineChildren a ->
                    let
                        arr =
                            toInlineArray a
                    in
                    case Array.get x arr of
                        Nothing ->
                            Nothing

                        Just n ->
                            case n of
                                Text tl ->
                                    let
                                        ( before, after ) =
                                            splitTextLeaf offset tl
                                    in
                                    Just
                                        ( node |> withChildNodes (inlineChildren (Array.set x (Text before) (Array.Extra.sliceUntil (x + 1) arr)))
                                        , node |> withChildNodes (inlineChildren (Array.set 0 (Text after) (Array.Extra.sliceFrom x arr)))
                                        )

                                InlineElement _ ->
                                    Just
                                        ( node |> withChildNodes (inlineChildren (Array.Extra.sliceUntil x arr))
                                        , node |> withChildNodes (inlineChildren (Array.Extra.sliceFrom x arr))
                                        )

                Leaf ->
                    Nothing


allRange : (Node -> Bool) -> Path -> Path -> Block -> Bool
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


anyRange : (Node -> Bool) -> Path -> Path -> Block -> Bool
anyRange pred start end root =
    not <| allRange (\x -> not <| pred x) start end root


findClosestBlockPath : Path -> Block -> Path
findClosestBlockPath path node =
    case nodeAt path node of
        Nothing ->
            []

        Just n ->
            case n of
                Block _ ->
                    path

                Inline _ ->
                    parent path


joinBlocks : Block -> Block -> Maybe Block
joinBlocks b1 b2 =
    case childNodes b1 of
        BlockChildren a1 ->
            case childNodes b2 of
                BlockChildren a2 ->
                    Just <| (b1 |> withChildNodes (blockChildren (Array.append (toBlockArray a1) (toBlockArray a2))))

                _ ->
                    Nothing

        InlineChildren a1 ->
            case childNodes b2 of
                InlineChildren a2 ->
                    Just <| (b1 |> withChildNodes (inlineChildren (Array.append (toInlineArray a1) (toInlineArray a2))))

                _ ->
                    Nothing

        Leaf ->
            Nothing


insertAfter : Path -> Fragment -> Block -> Result String Block
insertAfter path fragment root =
    case nodeAt path root of
        Nothing ->
            Err "There is no node at this path"

        Just node ->
            case node of
                Inline il ->
                    case fragment of
                        InlineLeafFragment a ->
                            let
                                newFragment =
                                    InlineLeafFragment <| Array.fromList (il :: Array.toList a)
                            in
                            replaceWithFragment path newFragment root

                        BlockNodeFragment _ ->
                            Err "I cannot insert a block node fragment into an inline leaf fragment"

                Block bn ->
                    case fragment of
                        BlockNodeFragment a ->
                            let
                                newFragment =
                                    BlockNodeFragment <| Array.fromList (bn :: Array.toList a)
                            in
                            replaceWithFragment path newFragment root

                        InlineLeafFragment _ ->
                            Err "I cannot insert an inline leaf fragment fragment into an block node fragment"


insertBefore : Path -> Fragment -> Block -> Result String Block
insertBefore path fragment root =
    case nodeAt path root of
        Nothing ->
            Err "There is no node at this path"

        Just node ->
            case node of
                Inline il ->
                    case fragment of
                        InlineLeafFragment a ->
                            let
                                newFragment =
                                    InlineLeafFragment <| Array.push il a
                            in
                            replaceWithFragment path newFragment root

                        BlockNodeFragment _ ->
                            Err "I cannot insert a block node fragment into an inline leaf fragment"

                Block bn ->
                    case fragment of
                        BlockNodeFragment a ->
                            let
                                newFragment =
                                    BlockNodeFragment <| Array.push bn a
                            in
                            replaceWithFragment path newFragment root

                        InlineLeafFragment _ ->
                            Err "I cannot insert an inline leaf fragment fragment into an block node fragment"
