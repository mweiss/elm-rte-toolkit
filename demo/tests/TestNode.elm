module TestNode exposing (..)

import Array
import Expect
import RichTextEditor.Model.Annotations exposing (selectable)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , ChildNodes(..)
        , InlineLeaf(..)
        , Path
        , blockArray
        , blockNode
        , blockNodeWithElement
        , element
        , elementFromBlockNode
        , elementFromInlineLeafParameters
        , elementWithAnnotations
        , emptyTextLeafParameters
        , inlineLeafArray
        , inlineLeafParameters
        , inlineLeafParametersWithElement
        , nameFromElement
        , text
        , textLeafParametersWithAnnotations
        , textLeafWithText
        , withText
        )
import RichTextEditor.Node
    exposing
        ( Fragment(..)
        , Node(..)
        , allRange
        , anyRange
        , concatMap
        , findAncestor
        , findBackwardFrom
        , findBackwardFromExclusive
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
import RichTextEditor.NodePath exposing (toString)
import RichTextEditor.Specs exposing (doc, horizontalRule, image, paragraph)
import Set
import Test exposing (Test, describe, test)


rootNode : BlockNode
rootNode =
    blockNode
        (element doc [] Set.empty)
        (blockArray <| Array.fromList [ pNode ])


pNode : BlockNode
pNode =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode1, textNode2 ]
        )


textNode1 : InlineLeaf
textNode1 =
    textLeafWithText "sample1"


textNode2 =
    textLeafWithText "sample2"


testNodeAt : Test
testNodeAt =
    describe "Tests the function which finds a node given a node path and a block node"
        [ test "Test that we can find the root node" <|
            \_ ->
                Expect.equal (Just (Block rootNode)) (nodeAt [] rootNode)
        , test "Test that we can find the p node" <|
            \_ ->
                Expect.equal (Just (Block pNode)) (nodeAt [ 0 ] rootNode)
        , test "Test that we can find the first text node" <|
            \_ ->
                Expect.equal (Just (Inline textNode1)) (nodeAt [ 0, 0 ] rootNode)
        , test "Test that we can find the second text node" <|
            \_ ->
                Expect.equal (Just (Inline textNode2)) (nodeAt [ 0, 1 ] rootNode)
        , test "Test that invalid paths return no result" <|
            \_ ->
                Expect.equal Nothing (nodeAt [ 0, 2 ] rootNode)
        , test "Test that invalid paths that are too long return no result" <|
            \_ ->
                Expect.equal Nothing (nodeAt [ 0, 0, 0 ] rootNode)
        ]


nodePathList : Path -> Node -> List Path -> List Path
nodePathList path _ list =
    path :: list


nodeNameOrTextValue : Path -> Node -> List String -> List String
nodeNameOrTextValue _ node list =
    (case node of
        Block bn ->
            nameFromElement (elementFromBlockNode bn)

        Inline il ->
            case il of
                TextLeaf tl ->
                    text tl

                InlineLeaf p ->
                    nameFromElement (elementFromInlineLeafParameters p)
    )
        :: list


testIndexedFoldr : Test
testIndexedFoldr =
    describe "Tests that indexFoldr works as expected"
        [ test "Test that the node paths are passed in as expected" <|
            \_ -> Expect.equal [ [], [ 0 ], [ 0, 0 ], [ 0, 1 ] ] (indexedFoldr nodePathList [] (Block rootNode))
        , test "Test that the nodes are passed in as expected" <|
            \_ -> Expect.equal [ "doc", "paragraph", "sample1", "sample2" ] (indexedFoldr nodeNameOrTextValue [] (Block rootNode))
        ]


testFoldr : Test
testFoldr =
    describe "Tests that foldr works as expected"
        [ test "Test that the nodes are passed in as expected" <|
            \_ -> Expect.equal [ "doc", "paragraph", "sample1", "sample2" ] (foldr (\x -> nodeNameOrTextValue [] x) [] (Block rootNode))
        ]


testIndexedFoldl : Test
testIndexedFoldl =
    describe "Tests that indexedFoldl works as expected"
        [ test "Test that the node paths are passed in as expected" <|
            \_ -> Expect.equal [ [ 0, 1 ], [ 0, 0 ], [ 0 ], [] ] (indexedFoldl nodePathList [] (Block rootNode))
        , test "Test that the nodes are passed in as expected" <|
            \_ -> Expect.equal [ "sample2", "sample1", "paragraph", "doc" ] (indexedFoldl nodeNameOrTextValue [] (Block rootNode))
        ]


testFoldl : Test
testFoldl =
    describe "Tests that foldl works as expected"
        [ test "Test that the nodes are passed in as expected" <|
            \_ -> Expect.equal [ "sample2", "sample1", "paragraph", "doc" ] (foldl (\x -> nodeNameOrTextValue [] x) [] (Block rootNode))
        ]


testIsSelectable : Test
testIsSelectable =
    describe "Tests that a node is selectable"
        [ test "Test that a text node is selectable" <|
            \_ -> Expect.equal True <| isSelectable (Inline (textLeafWithText ""))
        , test "Test that a element node with a selectable mark is selectable" <|
            \_ -> Expect.equal True <| isSelectable (Block (blockNode (element doc [] (Set.fromList [ selectable ])) Leaf))
        , test "Test that a element node without a selectable mark is not selectable" <|
            \_ -> Expect.equal False <| isSelectable (Block (blockNode (element doc [] Set.empty) Leaf))
        ]


setAnnotations : String -> Node -> Node
setAnnotations mark node =
    let
        annotations =
            Set.fromList [ mark ]
    in
    case node of
        Block bn ->
            let
                params =
                    elementFromBlockNode bn
            in
            Block (bn |> blockNodeWithElement (params |> elementWithAnnotations annotations))

        Inline il ->
            case il of
                TextLeaf tl ->
                    Inline (TextLeaf (tl |> textLeafParametersWithAnnotations annotations))

                InlineLeaf l ->
                    let
                        params =
                            elementFromInlineLeafParameters l
                    in
                    Inline (InlineLeaf (l |> inlineLeafParametersWithElement (params |> elementWithAnnotations annotations)))


dummyAnnotation =
    "__dummy__"


addDummyAnnotation : Node -> Node
addDummyAnnotation node =
    setAnnotations dummyAnnotation node


addPathAnnotation : Path -> Node -> Node
addPathAnnotation path node =
    let
        pathAnnotation =
            toString path
    in
    setAnnotations pathAnnotation node


rootNodeWithPathAnnotation =
    blockNode
        (element doc [] (Set.fromList [ "" ]))
        (blockArray <|
            Array.fromList [ pHtmlNodeWithPathAnnotation ]
        )


pHtmlNodeWithPathAnnotation =
    blockNode
        (element paragraph [] (Set.fromList [ "0" ]))
        (inlineLeafArray <|
            Array.fromList [ textNode1WithPathAnnotation, textNode2WithPathAnnotation ]
        )


textNode1WithPathAnnotation =
    TextLeaf
        (emptyTextLeafParameters
            |> withText "sample1"
            |> textLeafParametersWithAnnotations (Set.fromList [ "0:0" ])
        )


textNode2WithPathAnnotation =
    TextLeaf
        (emptyTextLeafParameters
            |> withText "sample2"
            |> textLeafParametersWithAnnotations (Set.fromList [ "0:1" ])
        )


testIndexedMap : Test
testIndexedMap =
    describe "Tests that indexedMap works as expected"
        [ test "Test that node paths are passed in correctly" <|
            \_ ->
                Expect.equal (Block rootNodeWithPathAnnotation)
                    (indexedMap addPathAnnotation (Block rootNode))
        ]


rootNodeWithSameAnnotation =
    blockNode
        (element doc [] (Set.fromList [ dummyAnnotation ]))
        (blockArray <|
            Array.fromList [ pHtmlNodeWithSameAnnotation ]
        )


pHtmlNodeWithSameAnnotation =
    blockNode
        (element paragraph [] (Set.fromList [ dummyAnnotation ]))
        (inlineLeafArray <|
            Array.fromList [ textNode1WithSameAnnotation, textNode2WithSameAnnotation ]
        )


textNode1WithSameAnnotation =
    TextLeaf
        (emptyTextLeafParameters
            |> withText "sample1"
            |> textLeafParametersWithAnnotations (Set.fromList [ dummyAnnotation ])
        )


textNode2WithSameAnnotation =
    TextLeaf
        (emptyTextLeafParameters
            |> withText "sample2"
            |> textLeafParametersWithAnnotations (Set.fromList [ dummyAnnotation ])
        )


testMap : Test
testMap =
    describe "Tests that map works as expected"
        [ test "Test that nodes are correctly modified" <|
            \_ ->
                Expect.equal (Block rootNodeWithSameAnnotation)
                    (map addDummyAnnotation (Block rootNode))
        ]


testFindTextBlockNodeAncestor : Test
testFindTextBlockNodeAncestor =
    describe "Tests that findTextBlockNodeAncestor works as expected"
        [ test "Tests that we can correct find a text block node ancestor" <|
            \_ ->
                Expect.equal (Just ( [ 0 ], pNode )) (findTextBlockNodeAncestor [ 0, 0 ] rootNode)
        , test "Tests that we return nothing if no text block can be found" <|
            \_ ->
                Expect.equal Nothing (findTextBlockNodeAncestor [ 0 ] rootNode)
        ]


testFindAncestor : Test
testFindAncestor =
    describe "Tests that findAncestor works as expected"
        [ test "Tests that we can return the root if it's an ancestor" <|
            \_ ->
                Expect.equal
                    (Just ( [], rootNode ))
                    (findAncestor
                        (\n -> nameFromElement (elementFromBlockNode n) == "doc")
                        [ 0, 0 ]
                        rootNode
                    )
        ]


findNodeAtPath : Path -> Path -> Node -> Bool
findNodeAtPath path1 path2 _ =
    path1 == path2


findNodeWithName : String -> Path -> Node -> Bool
findNodeWithName name _ node =
    case node of
        Block bn ->
            nameFromElement (elementFromBlockNode bn) == name

        Inline il ->
            case il of
                InlineLeaf l ->
                    nameFromElement (elementFromInlineLeafParameters l) == name

                _ ->
                    False


testFindBackwardFrom : Test
testFindBackwardFrom =
    describe "Tests that findBackwardFrom works as expected"
        [ test "Tests that the path is correctly passed in" <|
            \_ ->
                Expect.equal (Just ( [ 0, 0 ], Inline textNode1 ))
                    (findBackwardFrom (findNodeAtPath [ 0, 0 ]) [ 0, 1 ] rootNode)
        , test "Tests that the function includes the passed in path" <|
            \_ ->
                Expect.equal (Just ( [ 0, 0 ], Inline textNode1 ))
                    (findBackwardFrom (findNodeAtPath [ 0, 0 ]) [ 0, 0 ] rootNode)
        , test "Tests that the function returns Nothing if nothing is found" <|
            \_ ->
                Expect.equal Nothing
                    (findBackwardFrom (findNodeAtPath [ 1, 0 ]) [ 0, 1 ] rootNode)
        , test "Tests that the function passes in the node parameter correctly" <|
            \_ ->
                Expect.equal (Just ( [], Block rootNode ))
                    (findBackwardFrom (findNodeWithName "doc") [ 0, 1 ] rootNode)
        ]


testFindBackwardFromExclusive : Test
testFindBackwardFromExclusive =
    describe "Tests that findBackwardFromExclusive works as expected"
        [ test "Tests that the path is correctly passed in" <|
            \_ ->
                Expect.equal (Just ( [ 0, 0 ], Inline textNode1 ))
                    (findBackwardFromExclusive (findNodeAtPath [ 0, 0 ]) [ 0, 1 ] rootNode)
        , test "Tests that the function excludes the passed in path" <|
            \_ ->
                Expect.equal Nothing
                    (findBackwardFromExclusive (findNodeAtPath [ 0, 0 ]) [ 0, 0 ] rootNode)
        , test "Tests that the function returns Nothing if nothing is found" <|
            \_ ->
                Expect.equal Nothing
                    (findBackwardFromExclusive (findNodeAtPath [ 1, 0 ]) [ 0, 1 ] rootNode)
        , test "Tests that the function passes in the node parameter correctly" <|
            \_ ->
                Expect.equal (Just ( [], Block rootNode ))
                    (findBackwardFromExclusive (findNodeWithName "doc") [ 0, 1 ] rootNode)
        ]


testFindForwardFrom : Test
testFindForwardFrom =
    describe "Tests that findNodeForwardFrom works as expected"
        [ test "Tests that the path is correctly passed in" <|
            \_ ->
                Expect.equal (Just ( [ 0, 0 ], Inline textNode1 ))
                    (findForwardFrom (findNodeAtPath [ 0, 0 ]) [ 0 ] rootNode)
        , test "Tests that the function includes the passed in path" <|
            \_ ->
                Expect.equal (Just ( [ 0, 0 ], Inline textNode1 ))
                    (findForwardFrom (findNodeAtPath [ 0, 0 ]) [ 0, 0 ] rootNode)
        , test "Tests that the function returns Nothing if nothing is found" <|
            \_ ->
                Expect.equal Nothing
                    (findForwardFrom (findNodeAtPath [ 1, 0 ]) [ 0, 1 ] rootNode)
        , test "Tests that the function passes in the node parameter correctly" <|
            \_ ->
                Expect.equal (Just ( [], Block rootNode ))
                    (findForwardFrom (findNodeWithName "doc") [] rootNode)
        ]


testFindForwardFromExclusive : Test
testFindForwardFromExclusive =
    describe "Tests that findNodeForwardFromExclusive works as expected"
        [ test "Tests that the path is correctly passed in" <|
            \_ ->
                Expect.equal (Just ( [ 0, 0 ], Inline textNode1 ))
                    (findForwardFromExclusive (findNodeAtPath [ 0, 0 ]) [ 0 ] rootNode)
        , test "Tests that the function excludes the passed in path" <|
            \_ ->
                Expect.equal Nothing
                    (findForwardFromExclusive (findNodeAtPath [ 0, 0 ]) [ 0, 0 ] rootNode)
        , test "Tests that the function returns Nothing if nothing is found" <|
            \_ ->
                Expect.equal Nothing
                    (findForwardFromExclusive (findNodeAtPath [ 1, 0 ]) [ 0, 1 ] rootNode)
        , test "Tests that the function passes in the node parameter correctly" <|
            \_ ->
                Expect.equal (Just ( [ 0 ], Block pNode ))
                    (findForwardFromExclusive (findNodeWithName "paragraph") [] rootNode)
        ]


testNext : Test
testNext =
    describe "Tests that next works as expected"
        [ test "Tests that we receive the next element from root" <|
            \_ ->
                Expect.equal (Just ( [ 0 ], Block pNode ))
                    (next [] rootNode)
        , test "Tests that we receive nothing after the last node" <|
            \_ ->
                Expect.equal Nothing
                    (next [ 0, 1 ] rootNode)
        ]


testPrevious : Test
testPrevious =
    describe "Tests that previous works as expected"
        [ test "Tests that we receive the previous element from root" <|
            \_ ->
                Expect.equal (Just ( [], Block rootNode ))
                    (previous [ 0 ] rootNode)
        , test "Tests that we receive nothing before the root node" <|
            \_ ->
                Expect.equal Nothing
                    (previous [] rootNode)
        ]


removedRootNode =
    blockNode
        (element doc [] Set.empty)
        (blockArray <|
            Array.fromList [ removedPHtmlNode ]
        )


removedPHtmlNode =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode2 ]
        )


removedRootAll =
    blockNode
        (element doc [] Set.empty)
        (blockArray Array.empty)


removedPHtmlNodeAll =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray Array.empty)


removedRootNodeRemovedPNodeAll =
    blockNode
        (element doc [] Set.empty)
        (blockArray <|
            Array.fromList [ removedPHtmlNodeAll ]
        )


testRemoveNodeAndEmptyParents : Test
testRemoveNodeAndEmptyParents =
    describe "Tests that removeNodeAndEmptyParents works as expected"
        [ test "Tests that we can remove a text node properly" <|
            \_ ->
                Expect.equal removedRootNode
                    (removeNodeAndEmptyParents [ 0, 0 ] rootNode)
        , test "Tests that we remove parents properly" <|
            \_ ->
                Expect.equal removedRootAll
                    (removeNodeAndEmptyParents [ 0, 0 ] removedRootNode)
        ]


testRemoveInRange : Test
testRemoveInRange =
    describe "Tests that removeInRange works as expected"
        [ test "Tests that we remove elements we want" <|
            \_ ->
                Expect.equal removedRootAll
                    (removeInRange [ 0 ] [ 0 ] rootNode)
        , test "Tests that we remove elements we want, part ii" <|
            \_ ->
                Expect.equal removedRootNodeRemovedPNodeAll
                    (removeInRange [ 0, 0 ] [ 0, 1 ] rootNode)
        ]


replaceRootPNode =
    blockNode
        (element doc [] Set.empty)
        (blockArray <| Array.fromList [ replacePNode ])


replacePNode =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode2, textNode2 ]
        )


testReplace : Test
testReplace =
    describe "Tests that replace works as expected"
        [ test "Tests that we replace the element we want" <|
            \_ ->
                Expect.equal (Ok replaceRootPNode)
                    (replace [ 0, 0 ] (Inline textNode2) rootNode)
        ]


testReplaceWithFragment : Test
testReplaceWithFragment =
    describe "Tests that replaceWithFragment works as expected"
        [ test "Tests that we replace the element we want" <|
            \_ ->
                Expect.equal (Ok replaceRootPNode)
                    (replaceWithFragment [ 0, 0 ] (InlineLeafFragment <| Array.fromList [ textNode2 ]) rootNode)
        ]


testAllRange : Test
testAllRange =
    describe "Tests that allRange works as expected"
        [ test "Tests that an empty range returns true" <|
            \_ -> Expect.equal True <| allRange (\_ -> False) [ 0, 1 ] [ 0, 0 ] rootNode
        , test "Tests that a single node range works as expected" <|
            \_ -> Expect.equal True <| allRange (\node -> node == Block pNode) [ 0 ] [ 0 ] rootNode
        , test "Tests that a node range with one false returns False" <|
            \_ -> Expect.equal False <| allRange (\node -> node == Block pNode) [ 0 ] [ 0, 0 ] rootNode
        ]


testAnyRange : Test
testAnyRange =
    describe "Tests that anyRange works as expected"
        [ test "Tests that an empty range returns false" <|
            \_ -> Expect.equal False <| anyRange (\_ -> False) [ 0, 1 ] [ 0, 0 ] rootNode
        , test "Tests that a single node range works as expected" <|
            \_ -> Expect.equal True <| anyRange (\node -> node == Block pNode) [ 0 ] [ 0 ] rootNode
        , test "Tests that a node range with one true value returns True" <|
            \_ -> Expect.equal True <| anyRange (\node -> node == Block pNode) [ 0 ] [ 0, 0 ] rootNode
        , test "Tests that a node range with no true values returns False" <|
            \_ -> Expect.equal False <| anyRange (\_ -> False) [ 0 ] [ 0, 1 ] rootNode
        ]


doubleRoot =
    blockNode
        (element doc [] Set.empty)
        (blockArray <|
            Array.fromList [ doublePNode, doublePNode ]
        )


doublePNode =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode1, textNode1, textNode2, textNode2 ]
        )


testConcatMap : Test
testConcatMap =
    describe "Tests that concatMap works as expected"
        [ test "Tests that the identity function returns the same node" <|
            \_ ->
                Expect.equal rootNode <| concatMap (\node -> [ node ]) rootNode
        , test "Tests that the double function returns the expected tree" <|
            \_ -> Expect.equal doubleRoot <| concatMap (\node -> [ node, node ]) rootNode
        ]


nodeBeforeTextLeafSplit =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textLeafWithText "sam" ]
        )


nodeAfterTextLeafSplit =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textLeafWithText "ple1" ]
        )


nodeWithTextLeafToSplit =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode1 ]
        )


inlineImg =
    InlineLeaf <| inlineLeafParameters (element image [] Set.empty) []


nodeAfterInlineLeafSplit =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ inlineImg ]
        )


nodeBeforeInlineLeafSplit =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray Array.empty)


nodeWithInlineLeafToSplit =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <| Array.fromList [ inlineImg ])


testSplitBlockAtPathAndOffset : Test
testSplitBlockAtPathAndOffset =
    describe "Tests that testSplitBlockAtPathAndOffset works as expected"
        [ test "Tests that you cannot split a block at an invalid path" <|
            \_ -> Expect.equal Nothing <| splitBlockAtPathAndOffset [ 1 ] 0 rootNode
        , test "Tests that splitting a block node uses the offset to split before" <|
            \_ -> Expect.equal (Just ( removedRootAll, rootNode )) <| splitBlockAtPathAndOffset [] 0 rootNode
        , test "Tests that splitting a block node uses the offset to split after" <|
            \_ -> Expect.equal (Just ( rootNode, removedRootAll )) <| splitBlockAtPathAndOffset [] 1 rootNode
        , test "Tests that splitting a text leaf works correctly" <|
            \_ -> Expect.equal (Just ( nodeBeforeTextLeafSplit, nodeAfterTextLeafSplit )) <| splitBlockAtPathAndOffset [ 0 ] 3 nodeWithTextLeafToSplit
        , test "Tests that splitting an inline leaf works correctly" <|
            \_ -> Expect.equal (Just ( nodeBeforeInlineLeafSplit, nodeAfterInlineLeafSplit )) <| splitBlockAtPathAndOffset [ 0 ] 0 nodeWithInlineLeafToSplit
        ]


testSplitTextLeaf : Test
testSplitTextLeaf =
    describe "Tests that splitTextLeaf works as expected"
        [ test "Tests that splitting a text leaf works as expected" <|
            \_ ->
                Expect.equal
                    ( emptyTextLeafParameters |> withText "sam"
                    , emptyTextLeafParameters |> withText "ple1"
                    )
                <|
                    splitTextLeaf 3 (emptyTextLeafParameters |> withText "sample1")
        ]


hrNode : BlockNode
hrNode =
    blockNode
        (element horizontalRule [] Set.empty)
        Leaf


testFindLastPath : Test
testFindLastPath =
    describe "Tests that findLastPath works as expected"
        [ test "If this is a leaf node, it should return the root path" <|
            \_ ->
                Expect.equal ( [], Block hrNode ) (last hrNode)
        , test "If this is a node with inline children, it should return the last path correctly" <|
            \_ ->
                Expect.equal ( [ 1 ], Inline textNode2 ) (last pNode)
        , test "If this is a node with block children, it should return the last path correctly" <|
            \_ ->
                Expect.equal ( [ 0, 1 ], Inline textNode2 ) (last rootNode)
        ]


testFoldlRange : Test
testFoldlRange =
    describe "Tests that foldlRange works as expected"
        [ test "Test that a range query works correctly" <|
            \_ -> Expect.equal [ "sample2", "sample1", "paragraph" ] (foldlRange [ 0 ] [ 0, 1 ] (\x -> nodeNameOrTextValue [] x) [] rootNode)
        , test "Test that something outside of range does nothing" <|
            \_ -> Expect.equal [] (foldlRange [ 1 ] [ 1, 1 ] (\x -> nodeNameOrTextValue [] x) [] rootNode)
        , test "Test that an invalid range does nothing" <|
            \_ -> Expect.equal [] (foldlRange [ 1 ] [ 0 ] (\x -> nodeNameOrTextValue [] x) [] rootNode)
        ]


testFoldrRange : Test
testFoldrRange =
    describe "Tests that foldrRange works as expected"
        [ test "Test that a range query works correctly" <|
            \_ -> Expect.equal [ "paragraph", "sample1", "sample2" ] (foldrRange [ 0 ] [ 0, 1 ] (\x -> nodeNameOrTextValue [] x) [] rootNode)
        , test "Test that something outside of range does nothing" <|
            \_ -> Expect.equal [] (foldrRange [ 1 ] [ 1, 1 ] (\x -> nodeNameOrTextValue [] x) [] rootNode)
        , test "Test that an invalid range does nothing" <|
            \_ -> Expect.equal [] (foldrRange [ 1 ] [ 0 ] (\x -> nodeNameOrTextValue [] x) [] rootNode)
        ]


inlineInsertFragment =
    InlineLeafFragment (Array.fromList [ textNode1, textNode2 ])


blockInsertFragment =
    BlockNodeFragment (Array.fromList [ hrNode ])


expectedInsertBeforeBlock : BlockNode
expectedInsertBeforeBlock =
    blockNode
        (element doc [] Set.empty)
        (blockArray <| Array.fromList [ hrNode, pNode ])


expectedInsertBeforeInline : BlockNode
expectedInsertBeforeInline =
    blockNode
        (element doc [] Set.empty)
        (blockArray <| Array.fromList [ pNodeExpectedBeforeInline ])


pNodeExpectedBeforeInline : BlockNode
pNodeExpectedBeforeInline =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode1, textNode2, textNode1, textNode2 ]
        )


testInsertBefore : Test
testInsertBefore =
    describe "Tests that insertBefore works as expected"
        [ test "Make sure that we can insert an inline fragment" <|
            \_ ->
                Expect.equal (Ok expectedInsertBeforeInline)
                    (insertBefore [ 0, 0 ] inlineInsertFragment rootNode)
        , test "Make sure that we can insert a block fragment" <|
            \_ ->
                Expect.equal (Ok expectedInsertBeforeBlock)
                    (insertBefore [ 0 ] blockInsertFragment rootNode)
        , test "Invalid paths should result in an error" <|
            \_ ->
                Expect.equal
                    (Err "There is no node at this path")
                    (insertBefore [ 0, 9 ] inlineInsertFragment rootNode)
        , test "Trying to insert a block fragment into an inline array should result in an error" <|
            \_ ->
                Expect.equal
                    (Err "I cannot insert a block node fragment into an inline leaf fragment")
                    (insertBefore [ 0, 0 ] blockInsertFragment rootNode)
        , test "Trying to insert an inline fragment into an block array should result in an error" <|
            \_ ->
                Expect.equal
                    (Err "I cannot insert an inline leaf fragment fragment into an block node fragment")
                    (insertBefore [ 0 ] inlineInsertFragment rootNode)
        ]


expectedInsertAfterBlock : BlockNode
expectedInsertAfterBlock =
    blockNode
        (element doc [] Set.empty)
        (blockArray <| Array.fromList [ pNode, hrNode ])


expectedInsertAfterInline : BlockNode
expectedInsertAfterInline =
    blockNode
        (element doc [] Set.empty)
        (blockArray <| Array.fromList [ pNodeExpectedAfterInline ])


pNodeExpectedAfterInline : BlockNode
pNodeExpectedAfterInline =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode1, textNode1, textNode2, textNode2 ]
        )


testInsertAfter : Test
testInsertAfter =
    describe "Tests that insertAfter works as expected"
        [ test "Make sure that we can insert an inline fragment" <|
            \_ ->
                Expect.equal (Ok expectedInsertAfterInline)
                    (insertAfter [ 0, 0 ] inlineInsertFragment rootNode)
        , test "Make sure that we can insert a block fragment" <|
            \_ ->
                Expect.equal (Ok expectedInsertAfterBlock)
                    (insertAfter [ 0 ] blockInsertFragment rootNode)
        , test "Invalid paths should result in an error" <|
            \_ ->
                Expect.equal
                    (Err "There is no node at this path")
                    (insertAfter [ 0, 9 ] inlineInsertFragment rootNode)
        , test "Trying to insert a block fragment into an inline array should result in an error" <|
            \_ ->
                Expect.equal
                    (Err "I cannot insert a block node fragment into an inline leaf fragment")
                    (insertAfter [ 0, 0 ] blockInsertFragment rootNode)
        , test "Trying to insert an inline fragment into an block array should result in an error" <|
            \_ ->
                Expect.equal
                    (Err "I cannot insert an inline leaf fragment fragment into an block node fragment")
                    (insertAfter [ 0 ] inlineInsertFragment rootNode)
        ]


pNodeReverse : BlockNode
pNodeReverse =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode2, textNode1 ]
        )


pNodeExpectedJoin : BlockNode
pNodeExpectedJoin =
    blockNode
        (element paragraph [] Set.empty)
        (inlineLeafArray <|
            Array.fromList [ textNode1, textNode2, textNode2, textNode1 ]
        )


rootWithReversePNode : BlockNode
rootWithReversePNode =
    blockNode
        (element doc [] Set.empty)
        (blockArray <|
            Array.fromList [ pNodeReverse ]
        )


rootAfterJoin : BlockNode
rootAfterJoin =
    blockNode
        (element doc [] Set.empty)
        (blockArray <|
            Array.fromList [ pNode, pNodeReverse ]
        )


testJoinBlocks : Test
testJoinBlocks =
    describe "Tests that joinBlocks works as expected"
        [ test "Make sure that joining two blocks with inline content works as expected" <|
            \_ ->
                Expect.equal (Just pNodeExpectedJoin) (joinBlocks pNode pNodeReverse)
        , test "Make sure that joining two blocks with block content works as expected" <|
            \_ ->
                Expect.equal (Just rootAfterJoin) (joinBlocks rootNode rootWithReversePNode)
        , test "Joining a block with a leaf should result in nothing" <|
            \_ ->
                Expect.equal Nothing (joinBlocks hrNode rootNode)
        , test "Joining block children with inline children should result in nothing" <|
            \_ ->
                Expect.equal Nothing (joinBlocks pNode rootNode)
        ]
