module TestState exposing (..)

import Array
import Expect
import RichText.Definitions exposing (blockquote, bold, doc, listItem, markdown, paragraph)
import RichText.Model.Element as Element
import RichText.Model.Mark exposing (mark)
import RichText.Model.Node exposing (Block, Children(..), Inline, block, blockChildren, inlineChildren, markedText, plainText)
import RichText.Model.Selection exposing (caret)
import RichText.Model.State exposing (State, state)
import RichText.State exposing (reduce, validate)
import Test exposing (Test, describe, test)


reduceExample : State
reduceExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "te", plainText "xt" ])
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 2)


expectedReduceExample : State
expectedReduceExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 4)


reduceEmptyExample : State
reduceEmptyExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ markedText "" [ mark bold [] ]
                                , plainText "text"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 2)


expectedReduceEmptyExample : State
expectedReduceEmptyExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "text" ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 2)


validateExample : State
validateExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 2)


invalidGroupExample : State
invalidGroupExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element listItem [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 2)


invalidChildrenExample : State
invalidChildrenExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element blockquote [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 2)


testReduce : Test
testReduce =
    describe "Tests the reduce function"
        [ test "Tests that the example case works as expected" <|
            \_ ->
                Expect.equal expectedReduceExample (reduce reduceExample)
        , test "Tests that reducing an empty text block works as expected" <|
            \_ ->
                Expect.equal expectedReduceEmptyExample (reduce reduceEmptyExample)
        ]


testValidate : Test
testValidate =
    describe "Tests the validate function"
        [ test "Tests that the example case works as expected" <|
            \_ ->
                Expect.equal (Ok validateExample) (validate markdown validateExample)
        , test "groups are validated" <|
            \_ ->
                Expect.equal
                    (Err "Group list_item is not in allowed groups [block]")
                    (validate markdown invalidGroupExample)
        , test "children are validated" <|
            \_ ->
                Expect.equal
                    (Err "I was expecting textblock content type, but instead I got BlockNodeType")
                    (validate markdown invalidChildrenExample)
        ]
