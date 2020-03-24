module Model.TestNode exposing (..)

import Array
import Expect
import RichTextEditor.Model.Mark exposing (Mark, mark)
import RichTextEditor.Model.Node exposing (InlineTree(..), marksToMarkNodeList)
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
