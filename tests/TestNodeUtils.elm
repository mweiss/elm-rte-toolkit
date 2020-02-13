module TestNodeUtils exposing (..)

import Expect
import Rte.Model exposing (ChildNodes(..), EditorInlineLeaf(..))
import Rte.NodeUtils exposing (NodeResult(..), findNode)
import Test exposing (Test, describe, test)


rootNode =
    { parameters = { name = "div", attributes = [], marks = [] }
    , childNodes =
        BlockList
            [ pHtmlNode ]
    }


pHtmlNode =
    { parameters = { name = "div", attributes = [], marks = [] }
    , childNodes =
        InlineLeafList
            [ textNode1, textNode2 ]
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
                Expect.equal (BlockNodeResult rootNode) (findNode [] rootNode)
        , test "Test that we can find the p node" <|
            \_ ->
                Expect.equal (BlockNodeResult pHtmlNode) (findNode [ 0 ] rootNode)
        , test "Test that we can find the first text node" <|
            \_ ->
                Expect.equal (InlineLeafResult textNode1) (findNode [ 0, 0 ] rootNode)
        , test "Test that we can find the second text node" <|
            \_ ->
                Expect.equal (InlineLeafResult textNode2) (findNode [ 0, 1 ] rootNode)
        , test "Test that invalid paths return no result" <|
            \_ ->
                Expect.equal NoResult (findNode [ 0, 2 ] rootNode)
        , test "Test that invalid paths that are too long return no result" <|
            \_ ->
                Expect.equal NoResult (findNode [ 0, 0, 0 ] rootNode)
        ]
