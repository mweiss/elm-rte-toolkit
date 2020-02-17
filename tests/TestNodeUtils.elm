module TestNodeUtils exposing (..)

import Array
import Expect
import Rte.Model exposing (ChildNodes(..), EditorAttribute(..), EditorInlineLeaf(..), Mark, NodePath, selectableMark)
import Rte.NodePath exposing (toString)
import Rte.NodeUtils exposing (EditorNode(..), NodeResult(..), indexedFoldl, indexedFoldr, indexedMap, isSelectable, map, nodeAt)
import Test exposing (Test, describe, test)


rootNode =
    { parameters = { name = "div", attributes = [], marks = [] }
    , childNodes =
        BlockArray <|
            Array.fromList [ pHtmlNode ]
    }


pHtmlNode =
    { parameters = { name = "p", attributes = [], marks = [] }
    , childNodes =
        InlineLeafArray <|
            Array.fromList [ textNode1, textNode2 ]
    }


textNode1 =
    TextLeaf { marks = [], text = "sample1" }


textNode2 =
    TextLeaf { marks = [], text = "sample2" }


testNodeAt : Test
testNodeAt =
    describe "Tests the function which finds a node given a node path and a block node"
        [ test "Test that we can find the root node" <|
            \_ ->
                Expect.equal (Just (BlockNodeWrapper rootNode)) (nodeAt [] rootNode)
        , test "Test that we can find the p node" <|
            \_ ->
                Expect.equal (Just (BlockNodeWrapper pHtmlNode)) (nodeAt [ 0 ] rootNode)
        , test "Test that we can find the first text node" <|
            \_ ->
                Expect.equal (Just (InlineLeafWrapper textNode1)) (nodeAt [ 0, 0 ] rootNode)
        , test "Test that we can find the second text node" <|
            \_ ->
                Expect.equal (Just (InlineLeafWrapper textNode2)) (nodeAt [ 0, 1 ] rootNode)
        , test "Test that invalid paths return no result" <|
            \_ ->
                Expect.equal Nothing (nodeAt [ 0, 2 ] rootNode)
        , test "Test that invalid paths that are too long return no result" <|
            \_ ->
                Expect.equal Nothing (nodeAt [ 0, 0, 0 ] rootNode)
        ]


nodePathList : NodePath -> EditorNode -> List NodePath -> List NodePath
nodePathList path _ list =
    path :: list


nodeNameOrTextValue : NodePath -> EditorNode -> List String -> List String
nodeNameOrTextValue _ node list =
    (case node of
        BlockNodeWrapper bn ->
            bn.parameters.name

        InlineLeafWrapper il ->
            case il of
                TextLeaf tl ->
                    tl.text

                InlineLeaf p ->
                    p.name
    )
        :: list


testIndexedFoldr : Test
testIndexedFoldr =
    describe "Tests that indexFoldr works as expected"
        [ test "Test that the node paths are passed in as expected" <|
            \_ -> Expect.equal [ [], [ 0 ], [ 0, 0 ], [ 0, 1 ] ] (indexedFoldr nodePathList [] (BlockNodeWrapper rootNode))
        , test "Test that the nodes are passed in as expected" <|
            \_ -> Expect.equal [ "div", "p", "sample1", "sample2" ] (indexedFoldr nodeNameOrTextValue [] (BlockNodeWrapper rootNode))
        ]


testFoldr : Test
testFoldr =
    describe "Tests that foldr works as expected"
        [ test "Test that the nodes are passed in as expected" <|
            \_ -> Expect.equal [ "div", "p", "sample1", "sample2" ] (indexedFoldr nodeNameOrTextValue [] (BlockNodeWrapper rootNode))
        ]


testIndexedFoldl : Test
testIndexedFoldl =
    describe "Tests that indexedFoldl works as expected"
        [ test "Test that the node paths are passed in as expected" <|
            \_ -> Expect.equal [ [ 0, 1 ], [ 0, 0 ], [ 0 ], [] ] (indexedFoldl nodePathList [] (BlockNodeWrapper rootNode))
        , test "Test that the nodes are passed in as expected" <|
            \_ -> Expect.equal [ "sample2", "sample1", "p", "div" ] (indexedFoldl nodeNameOrTextValue [] (BlockNodeWrapper rootNode))
        ]


testFoldl : Test
testFoldl =
    describe "Tests that foldl works as expected"
        [ test "Test that the nodes are passed in as expected" <|
            \_ -> Expect.equal [ "sample2", "sample1", "p", "div" ] (indexedFoldl nodeNameOrTextValue [] (BlockNodeWrapper rootNode))
        ]


testIsSelectable : Test
testIsSelectable =
    describe "Tests that a node is selectable"
        [ test "Test that a text node is selectable" <|
            \_ -> Expect.equal True <| isSelectable (InlineLeafWrapper (TextLeaf { text = "", marks = [] }))
        , test "Test that a element node with a selectable mark is selectable" <|
            \_ -> Expect.equal True <| isSelectable (BlockNodeWrapper { parameters = { name = "div", attributes = [], marks = [ selectableMark ] }, childNodes = Leaf })
        , test "Test that a element node without a selectable mark is not selectable" <|
            \_ -> Expect.equal False <| isSelectable (BlockNodeWrapper { parameters = { name = "div", attributes = [], marks = [] }, childNodes = Leaf })
        ]


setMarks : Mark -> EditorNode -> EditorNode
setMarks mark node =
    let
        pathMarks =
            [ mark ]
    in
    case node of
        BlockNodeWrapper bn ->
            let
                params =
                    bn.parameters
            in
            BlockNodeWrapper { bn | parameters = { params | marks = pathMarks } }

        InlineLeafWrapper il ->
            case il of
                TextLeaf tl ->
                    InlineLeafWrapper (TextLeaf { tl | marks = pathMarks })

                InlineLeaf l ->
                    InlineLeafWrapper (InlineLeaf { l | marks = pathMarks })


dummyMark =
    Mark "dummy" []


addDummyMark : EditorNode -> EditorNode
addDummyMark node =
    setMarks dummyMark node


addPathMark : NodePath -> EditorNode -> EditorNode
addPathMark path node =
    let
        pathMark =
            Mark "path" [ StringAttribute "value" <| toString path ]
    in
    setMarks pathMark node


rootNodeWithPathMarks =
    { parameters = { name = "div", attributes = [], marks = [ Mark "path" [ StringAttribute "value" "" ] ] }
    , childNodes =
        BlockArray <|
            Array.fromList [ pHtmlNodeWithPathMarks ]
    }


pHtmlNodeWithPathMarks =
    { parameters = { name = "p", attributes = [], marks = [ Mark "path" [ StringAttribute "value" "0" ] ] }
    , childNodes =
        InlineLeafArray <|
            Array.fromList [ textNode1WithPathMarks, textNode2WithPathMarks ]
    }


textNode1WithPathMarks =
    TextLeaf { marks = [ Mark "path" [ StringAttribute "value" "0:0" ] ], text = "sample1" }


textNode2WithPathMarks =
    TextLeaf { marks = [ Mark "path" [ StringAttribute "value" "0:1" ] ], text = "sample2" }


testIndexedMap : Test
testIndexedMap =
    describe "Tests that indexedMap works as expected"
        [ test "Test that node paths are passed in correctly" <|
            \_ ->
                Expect.equal (BlockNodeWrapper rootNodeWithPathMarks)
                    (indexedMap addPathMark (BlockNodeWrapper rootNode))
        ]


rootNodeWithSameMark =
    { parameters = { name = "div", attributes = [], marks = [ dummyMark ] }
    , childNodes =
        BlockArray <|
            Array.fromList [ pHtmlNodeWithSameMark ]
    }


pHtmlNodeWithSameMark =
    { parameters = { name = "p", attributes = [], marks = [ dummyMark ] }
    , childNodes =
        InlineLeafArray <|
            Array.fromList [ textNode1WithSameMark, textNode2WithSameMark ]
    }


textNode1WithSameMark =
    TextLeaf { marks = [ dummyMark ], text = "sample1" }


textNode2WithSameMark =
    TextLeaf { marks = [ dummyMark ], text = "sample2" }


testMap : Test
testMap =
    describe "Tests that map works as expected"
        [ test "Test that nodes are correctly modified" <|
            \_ ->
                Expect.equal (BlockNodeWrapper rootNodeWithSameMark)
                    (map addDummyMark (BlockNodeWrapper rootNode))
        ]
