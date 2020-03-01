module TestMarks exposing (..)

import Expect
import Rte.Marks exposing (MarkNode(..), marksToMarkNodeList)
import Rte.Model exposing (Mark)
import Test exposing (Test, describe, test)


mark1 =
    Mark "mark1" []


mark2 =
    Mark "mark2" []


mark3 =
    Mark "mark3" []


testMarkLists =
    [ [ mark1, mark2 ], [ mark1, mark2, mark3 ], [ mark2 ], [] ]


expectedMarkNodes =
    [ Node { mark = mark1, start = 0, end = 1 }
        [ Node { mark = mark2, start = 0, end = 1 }
            [ Leaf { start = 0, end = 0 }
            , Node
                { mark = mark3, start = 1, end = 1 }
                [ Leaf { start = 1, end = 1 } ]
            ]
        ]
    , Node { mark = mark2, start = 2, end = 2 } [ Leaf { start = 2, end = 2 } ]
    , Leaf { start = 3, end = 3 }
    ]


testMarksToMarkNodeList : Test
testMarksToMarkNodeList =
    describe "Tests that marksToMarkNodeList works as expected"
        [ test "Tests that the output of marksToMarkNodeList is correct" <|
            \_ ->
                Expect.equal expectedMarkNodes (marksToMarkNodeList testMarkLists)
        ]
