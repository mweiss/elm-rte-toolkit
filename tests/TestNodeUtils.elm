module TestNodeUtils exposing (..)

import Array
import Expect
import Rte.Model exposing (ChildNodes(..), EditorInlineLeaf(..))
import Rte.NodeUtils exposing (EditorNode(..), NodeResult(..), nodeAt)
import Test exposing (Test, describe, test)


rootNode =
    { parameters = { name = "div", attributes = [], marks = [] }
    , childNodes =
        BlockArray <|
            Array.fromList [ pHtmlNode ]
    }


pHtmlNode =
    { parameters = { name = "div", attributes = [], marks = [] }
    , childNodes =
        InlineLeafArray <|
            Array.fromList [ textNode1, textNode2 ]
    }


textNode1 =
    TextLeaf { marks = [], text = "sample1" }


textNode2 =
    TextLeaf { marks = [], text = "sample2" }


testFindNode : Test
testFindNode =
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
