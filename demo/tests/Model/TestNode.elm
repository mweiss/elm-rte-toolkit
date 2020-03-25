module Model.TestNode exposing (..)

import Array
import Expect
import RichText.Model.Mark exposing (Mark, mark)
import RichText.Model.Node
    exposing
        ( InlineTree(..)
        , commonAncestor
        , decrement
        , increment
        , marksToMarkNodeList
        , parent
        , toString
        )
import SimpleSpec exposing (bold, italic, strikethrough)
import Test exposing (Test, describe, test)


mark1 =
    mark bold []


mark2 =
    mark italic []


mark3 =
    mark strikethrough []


testMarkLists =
    [ [ mark1, mark2 ], [ mark1, mark2, mark3 ], [ mark2 ], [] ]


expectedMarkNodes =
    Array.fromList <|
        [ MarkNode
            { mark = mark1
            , children =
                Array.fromList <|
                    [ MarkNode
                        { mark = mark2
                        , children =
                            Array.fromList <|
                                [ LeafNode 0
                                , MarkNode
                                    { mark = mark3
                                    , children =
                                        Array.fromList <|
                                            [ LeafNode 1 ]
                                    }
                                ]
                        }
                    ]
            }
        , MarkNode { mark = mark2, children = Array.fromList <| [ LeafNode 2 ] }
        , LeafNode 3
        ]


testNoMarksList =
    [ [], [], [], [] ]


expectedNoMarksList =
    Array.fromList <| [ LeafNode 0, LeafNode 1, LeafNode 2, LeafNode 3 ]


testMarksToMarkNodeList : Test
testMarksToMarkNodeList =
    describe "Tests that marksToMarkNodeList works as expected"
        [ test "Tests that the output of marksToMarkNodeList is correct" <|
            \_ ->
                Expect.equal expectedMarkNodes (marksToMarkNodeList testMarkLists)
        , test "Tests that the output of no marks is correct" <|
            \_ ->
                Expect.equal expectedNoMarksList (marksToMarkNodeList testNoMarksList)
        ]


testCommonAncestor : Test
testCommonAncestor =
    describe "Tests that commonAncestor works as expected"
        [ test "Test that the we can find the common ancestor of root and another path" <|
            \_ -> Expect.equal [] (commonAncestor [] [ 0, 1 ])
        , test "Test that the we can find the common ancestor of parent and another path" <|
            \_ -> Expect.equal [ 0 ] (commonAncestor [ 0 ] [ 0, 1 ])
        , test "Test that the we can find the common ancestor of two siblings" <|
            \_ -> Expect.equal [ 0 ] (commonAncestor [ 0, 2 ] [ 0, 1 ])
        , test "Test that the we can find the common of two long paths" <|
            \_ -> Expect.equal [ 0, 1 ] (commonAncestor [ 0, 1, 2, 3, 4 ] [ 0, 1, 3, 2, 4 ])
        ]


testDecrement : Test
testDecrement =
    describe "Tests that decrement works as expected"
        [ test "Decrementing the root path gives the root path" <|
            \_ -> Expect.equal [] (decrement [])
        , test "Decrementing a path should work as expected" <|
            \_ -> Expect.equal [ 0, 1 ] (decrement [ 0, 2 ])
        , test "Decrementing can go negative" <|
            \_ -> Expect.equal [ 0, -1 ] (decrement [ 0, 0 ])
        ]


testIncrement : Test
testIncrement =
    describe "Tests that increment works as expected"
        [ test "Incrementing the root path gives the root path" <|
            \_ -> Expect.equal [] (increment [])
        , test "Incrementing a path should work as expected" <|
            \_ -> Expect.equal [ 0, 2 ] (increment [ 0, 1 ])
        ]


testParent : Test
testParent =
    describe "Tests that parent works as expected"
        [ test "The parent of the root path is the root path" <|
            \_ -> Expect.equal [] (parent [])
        , test "The parent should be the same list with the last element removed" <|
            \_ -> Expect.equal [ 1, 2, 3 ] (parent [ 1, 2, 3, 4 ])
        ]


testToString : Test
testToString =
    describe "Tests that toString works as expected"
        [ test "The root path toString is the empty string" <|
            \_ -> Expect.equal "" (toString [])
        , test "The path toString should work for a path of length 1" <|
            \_ -> Expect.equal "1" (toString [ 1 ])
        , test "The path toString should work for a path of length > 1" <|
            \_ -> Expect.equal "1:3:0" (toString [ 1, 3, 0 ])
        ]
